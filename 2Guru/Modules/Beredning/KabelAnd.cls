 /*------------------------------------------------------------------------
    File        : KabelAnd
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Tue Sep 15 14:12:12 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/
USING Progress.Windows.Form.
  

CLASS Modules.Beredning.KabelAnd INHERITS Form   : 
   &Scoped-define PUBLIC PUBLIC
   
   {BEREDTH.i}
   DEFINE PRIVATE VARIABLE AddButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE AddRemoveStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE bindingSource1 AS Progress.Data.BindingSource NO-UNDO.
   DEFINE PRIVATE VARIABLE BTN_ID AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE BTN_KALK AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE BTN_MTRL AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE BTN_SKAP AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonUpp AS Controls.ButtonUpDown NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonOK AS Infragistics.Win.Misc.UltraButton NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonNy AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonNer AS Controls.ButtonUpDown NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonBort AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE ClearButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlDataDb                 AS Modules.Beredning.KabelAndDb                                      NO-UNDO.
   DEFINE PRIVATE VARIABLE GridBerMtrl AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE GridKonMtrl AS Controls.Grid NO-UNDO.
   DEFINE VARIABLE gruppkod AS INTEGER NO-UNDO.
   DEFINE VARIABLE comboList AS Infragistics.Win.ValueList NO-UNDO EXTENT 6.
   DEFINE PRIVATE VARIABLE PopBerMtrlOK AS Infragistics.Win.Misc.UltraButton NO-UNDO.
   DEFINE PRIVATE VARIABLE PopMtrlOK AS Infragistics.Win.Misc.UltraButton NO-UNDO.
   DEFINE PUBLIC VARIABLE PopupBerMtrl AS Infragistics.Win.Misc.UltraPopupControlContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE PopBerMtrlPanel AS Controls.PopPanel NO-UNDO.
   DEFINE PUBLIC VARIABLE PopupMtrl AS Infragistics.Win.Misc.UltraPopupControlContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE PopMtrlPanel AS Controls.PopPanel NO-UNDO.
   DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
   DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
   DEFINE PRIVATE VARIABLE RemoveButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE rubrikvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE Bereh AS HANDLE NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PRIVATE VARIABLE HmtRubrikerlista             AS Controls.GridRubrikLista                                        NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE GridKabAnd AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridB6 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridB5 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridB4 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridB3 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridB2 AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridB1 AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContaineB1B2B4B5 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerAddRemo AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerButtunOk AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerNyBort AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE SplitContainerPopBerMtrl AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerPopMtrlPanel AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerStart AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerValjKonst AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerValdaKon AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerKonst AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerKabelAnd AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerB3B6 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerB2B5 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerB1B4 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC  VARIABLE beforekolnamn  AS CHARACTER NO-UNDO.
   DEFINE PUBLIC  VARIABLE beforekolvarde AS CHARACTER NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTextEditorOrdning AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
   
   DEFINE VARIABLE synkkoll AS LOGICAL NO-UNDO.
   DEFINE VARIABLE GridRwid AS ROWID EXTENT 6 NO-UNDO. 
	CONSTRUCTOR PUBLIC KabelAnd(r AS Guru.Root, rubrikvarin AS CHARACTER, gruppkodin AS INTEGER ):
     
		SUPER().
		THIS-OBJECT:ROOT = r.
InitializeComponent ( ).
		THIS-OBJECT:Root:DatabaseManager:KabelAnd:ControllKabelAnd = THIS-OBJECT.
		rubrikvar = rubrikvarin.
		gruppkod = gruppkodin.
		THIS-OBJECT:HmtRubrikerlista = NEW Controls.GridRubrikLista().
		
		THIS-OBJECT:ControlDataDb = THIS-OBJECT:Root:DatabaseManager:KabelAnd.
		THIS-OBJECT:ControlDataDb:InitializeTTs(gruppkod).
		{BEREDTDBH.I ControlDataDb}
      
		THIS-OBJECT:CreateForm().
		
	  THIS-OBJECT:SIZE = THIS-OBJECT:Root:WindowManager:Wnd:size.
	  CATCH e AS Progress.Lang.Error:
			UNDO, THROW e.
		END CATCH.		

	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC KabelAnd( ):
      DELETE OBJECT ROOT NO-ERROR.
		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.
      
	END DESTRUCTOR.	



	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID AddButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ButtonNy_Click(?,?).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID BTN_ID_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		Guru.GlobalaVariabler:SkapGaTill = "ID".
		THIS-OBJECT:ButtonOK_Click( INPUT ?, INPUT ? ).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID BTN_KALK_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		Guru.GlobalaVariabler:SkapGaTill = "KALK".
      THIS-OBJECT:ButtonOK_Click( INPUT ?, INPUT ? ).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID BTN_MTRL_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		Guru.GlobalaVariabler:SkapGaTill = "MTRL".
      THIS-OBJECT:ButtonOK_Click( INPUT ?, INPUT ? ).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID BTN_SKAP_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		Guru.GlobalaVariabler:SkapGaTill = "SKAP".
      THIS-OBJECT:ButtonOK_Click( INPUT ?, INPUT ? ).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonBort_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
		
		IF THIS-OBJECT:ButtonBort:RadFromGrid() = TRUE THEN DO:   
		   THIS-OBJECT:KskKon_valTTh:FIND-FIRST("WHERE TTRECID = " + STRING(THIS-OBJECT:GridKabAnd:ActiveRow:Cells["TTRECID"]:VALUE ), NO-LOCK) NO-ERROR.
         IF THIS-OBJECT:ControlDataDb:InkopKoll() = TRUE THEN DO:
            rrr = System.Windows.Forms.MessageBox:Show(Guru.konstanter:globalroot:LanguageManager:GetStringAsMessage(194), "").
            RETURN. 
         END. 
		   rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
         IF rrr:ToString() NE "Yes" THEN RETURN.
         THIS-OBJECT:ControlDataDb:BortMtrl().
         THIS-OBJECT:ButtonBort:GuruOver().
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
   METHOD PRIVATE VOID ButtonNer_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:UppNerKabel(FALSE).
      RETURN.

   END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonNy_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      IF THIS-OBJECT:ButtonNy:RadFromGrid() = TRUE THEN DO:
         IF THIS-OBJECT:GridKabAnd:ActiveRow NE ? THEN THIS-OBJECT:GridKabAnd:ActiveRow:Selected = TRUE.
         THIS-OBJECT:ButtonNy:GuruOver().
         THIS-OBJECT:ControlDataDb:NyKabel(). 
         THIS-OBJECT:GridKabAnd:GuruReOpen().
         IF THIS-OBJECT:GridKabAnd:SELECTED:Rows:Count = 1 THEN THIS-OBJECT:ButtonNy:GuruTogrid:GuruCellvaljare("KABELADRESS").
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
	METHOD PRIVATE VOID ButtonOK_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:CLOSE().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonUpp_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:UppNerKabel(TRUE).
		RETURN.

	END METHOD.

	  
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ClearButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:AvmarkeraB1(?,?).		
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridB2_AfterSelectChange( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.AfterSelectChangeEventArgs ):
	   DEFINE VARIABLE kvalkodvar3 AS CHARACTER NO-UNDO.
	   DEFINE VARIABLE kvalkodvar4 AS CHARACTER NO-UNDO.
	   DEFINE VARIABLE kvalkodvar5 AS CHARACTER NO-UNDO.	
	   IF THIS-OBJECT:UnderSynkKoll(TRUE) = TRUE THEN DO:
         IF THIS-OBJECT:andkonstvaltemp2TTh:AVAILABLE THEN DO:
            IF THIS-OBJECT:andkonstvaltemp3TTh:AVAILABLE THEN kvalkodvar3 = THIS-OBJECT:andkonstvaltemp3TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE.
            ELSE kvalkodvar3 = ?.
            IF gruppkod = 0 THEN DO:
               THIS-OBJECT:ControlDataDb:andbrwb2(INPUT THIS-OBJECT:andkonstvaltemp2TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE).
               THIS-OBJECT:GridB3:GuruReOpen().
               IF kvalkodvar3 = ? THEN DO:
                  THIS-OBJECT:andkonstvaltemp3TTh:BUFFER-RELEASE().
                  THIS-OBJECT:GridB3:GuruAvmarkeraTot().
               END.
               ELSE DO:
                  THIS-OBJECT:andkonstvaltemp3TTh:FIND-FIRST("WHERE KVALKOD = '" + kvalkodvar3 + "'" ) NO-ERROR.
                  IF THIS-OBJECT:andkonstvaltemp3TTh:AVAILABLE THEN THIS-OBJECT:GridB3:GuruRepositionto(THIS-OBJECT:andkonstvaltemp3TTh:ROWID).
                  ELSE kvalkodvar3 = ?.
               END.
            END.
            ELSE DO:
               IF THIS-OBJECT:andkonstvaltemp4TTh:AVAILABLE THEN kvalkodvar4 = THIS-OBJECT:andkonstvaltemp4TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE.
               ELSE kvalkodvar4 = ?.
               THIS-OBJECT:ControlDataDb:andbrwb2(INPUT THIS-OBJECT:andkonstvaltemp2TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE).
               THIS-OBJECT:GridB4:GuruReOpen().
               IF kvalkodvar4 = ? OR kvalkodvar4 = "" THEN DO:
                  THIS-OBJECT:andkonstvaltemp4TTh:BUFFER-RELEASE().
                  THIS-OBJECT:GridB4:GuruAvmarkeraTot().
               END.
               ELSE DO:
                  THIS-OBJECT:andkonstvaltemp4TTh:FIND-FIRST("WHERE KVALKOD = '" + kvalkodvar4 + "'" ) NO-ERROR.
                  IF THIS-OBJECT:andkonstvaltemp4TTh:AVAILABLE THEN THIS-OBJECT:GridB4:GuruRepositionto(THIS-OBJECT:andkonstvaltemp4TTh:ROWID).
                  ELSE kvalkodvar4 = ?.
               END.
            END.   
         END.
           
      END.
      ELSE IF THIS-OBJECT:UnderSynkKollFo(TRUE) = TRUE THEN DO:
         IF  Guru.Konstanter:varforetypval[64] = 1 AND gruppkod = 0 THEN DO:
            
            THIS-OBJECT:ControlDataDb:andbrwb2(INPUT THIS-OBJECT:andkonstvaltemp2TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE).
            
            THIS-OBJECT:GridB3:GuruReOpen().
            IF kvalkodvar3 = ? THEN DO:
               THIS-OBJECT:andkonstvaltemp3TTh:BUFFER-RELEASE().
               THIS-OBJECT:GridB3:GuruAvmarkeraTot().
            END.
            ELSE DO:
               THIS-OBJECT:andkonstvaltemp3TTh:FIND-FIRST("WHERE KVALKOD = '" + kvalkodvar3 + "'" ) NO-ERROR.
               IF THIS-OBJECT:andkonstvaltemp3TTh:AVAILABLE THEN THIS-OBJECT:GridB3:GuruRepositionto(THIS-OBJECT:andkonstvaltemp3TTh:ROWID).
               ELSE kvalkodvar3 = ?.
            END.
         END.     
         ELSE IF gruppkod = 0 THEN RETURN.      
         IF THIS-OBJECT:andkonstvaltemp2TTh:AVAILABLE THEN DO:
            IF THIS-OBJECT:andkonstvaltemp5TTh:AVAILABLE THEN kvalkodvar5 = THIS-OBJECT:andkonstvaltemp5TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE.
            ELSE kvalkodvar5 = ?.
            IF THIS-OBJECT:andkonstvaltemp4TTh:AVAILABLE THEN kvalkodvar4 = THIS-OBJECT:andkonstvaltemp4TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE.
            ELSE kvalkodvar4 = ?.
            THIS-OBJECT:ControlDataDb:andbrwb2(INPUT THIS-OBJECT:andkonstvaltemp2TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE).
            
            THIS-OBJECT:GridB5:GuruReOpen().
            IF kvalkodvar5 = ? THEN DO:
               THIS-OBJECT:andkonstvaltemp5TTh:BUFFER-RELEASE().
               THIS-OBJECT:GridB5:GuruAvmarkeraTot().
            END.
            ELSE DO:
               THIS-OBJECT:andkonstvaltemp5TTh:FIND-FIRST("WHERE KVALKOD = '" + kvalkodvar5 + "'" ) NO-ERROR.
               IF THIS-OBJECT:andkonstvaltemp5TTh:AVAILABLE THEN THIS-OBJECT:GridB5:GuruRepositionto(THIS-OBJECT:andkonstvaltemp5TTh:ROWID).
               ELSE kvalkodvar5 = ?.
            END.
            THIS-OBJECT:GridB4:GuruReOpen().
            IF kvalkodvar4 = ? OR kvalkodvar4 = "" THEN DO:
               THIS-OBJECT:andkonstvaltemp4TTh:BUFFER-RELEASE().
               THIS-OBJECT:GridB4:GuruAvmarkeraTot().
            END.
            ELSE DO:
               THIS-OBJECT:andkonstvaltemp4TTh:FIND-FIRST("WHERE KVALKOD = '" + kvalkodvar4 + "'" ) NO-ERROR.
               IF THIS-OBJECT:andkonstvaltemp4TTh:AVAILABLE THEN THIS-OBJECT:GridB4:GuruRepositionto(THIS-OBJECT:andkonstvaltemp4TTh:ROWID).
               ELSE kvalkodvar4 = ?.
            END.
            
            THIS-OBJECT:andkonstvaltemp3TTh:BUFFER-RELEASE().
            THIS-OBJECT:GridB3:GuruAvmarkeraTot().
            IF THIS-OBJECT:andkonstvaltemp4TTh:AVAILABLE THEN kvalkodvar4 = THIS-OBJECT:andkonstvaltemp4TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE.
            ELSE kvalkodvar4 = ?.
            
         END.                
      END.   
		RETURN.

	END METHOD.

	
	
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridB3_AfterSelectChange( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.AfterSelectChangeEventArgs ):
		DEFINE VARIABLE kvalkodvar4 AS CHARACTER NO-UNDO.
		DEFINE VARIABLE kvalkodvar5 AS CHARACTER NO-UNDO.
      
      IF THIS-OBJECT:UnderSynkKoll(TRUE) = TRUE THEN DO:
         IF THIS-OBJECT:andkonstvaltemp3TTh:AVAILABLE THEN DO:
            IF gruppkod = 0 THEN DO:
               IF THIS-OBJECT:andkonstvaltemp5TTh:AVAILABLE THEN kvalkodvar5 = THIS-OBJECT:andkonstvaltemp5TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE.
               ELSE kvalkodvar5 = ?.
               THIS-OBJECT:ControlDataDb:andbrwb3(INPUT THIS-OBJECT:andkonstvaltemp3TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE).
               THIS-OBJECT:GridB5:GuruReOpen().
               IF kvalkodvar5 = ? THEN DO:
                  THIS-OBJECT:andkonstvaltemp5TTh:BUFFER-RELEASE().
                  THIS-OBJECT:GridB5:GuruAvmarkeraTot().
               END.
               ELSE DO:
                  THIS-OBJECT:andkonstvaltemp5TTh:FIND-FIRST("WHERE KVALKOD = '" + kvalkodvar5 + "'" ) NO-ERROR.
                  IF THIS-OBJECT:andkonstvaltemp5TTh:AVAILABLE THEN THIS-OBJECT:GridB5:GuruRepositionto(THIS-OBJECT:andkonstvaltemp5TTh:ROWID).
                  ELSE kvalkodvar5 = ?.
               END.
            END.   
            ELSE DO:
               IF THIS-OBJECT:andkonstvaltemp4TTh:AVAILABLE THEN kvalkodvar4 = THIS-OBJECT:andkonstvaltemp4TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE.
               ELSE kvalkodvar4 = ?.
               THIS-OBJECT:ControlDataDb:andbrwb3(INPUT THIS-OBJECT:andkonstvaltemp3TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE).
               THIS-OBJECT:GridB4:GuruReOpen().           
               IF kvalkodvar4 = ? OR kvalkodvar4 = ""  THEN DO:
                  THIS-OBJECT:andkonstvaltemp4TTh:BUFFER-RELEASE().
                  THIS-OBJECT:GridB4:GuruAvmarkeraTot().
               END. 
               ELSE DO:
                  THIS-OBJECT:andkonstvaltemp4TTh:FIND-FIRST("WHERE KVALKOD = '" + kvalkodvar4 + "'" ) NO-ERROR.
                  IF THIS-OBJECT:andkonstvaltemp4TTh:AVAILABLE THEN THIS-OBJECT:GridB4:GuruRepositionto(THIS-OBJECT:andkonstvaltemp4TTh:ROWID).
                  ELSE kvalkodvar4 = ?.
                  IF kvalkodvar4 = ? OR kvalkodvar4 = "" THEN DO:
                     THIS-OBJECT:andkonstvaltemp4TTh:BUFFER-RELEASE().
                     THIS-OBJECT:GridB4:GuruAvmarkeraTot().
                  END. 
               END.                        
            END.
         END.   
      END.
      ELSE IF THIS-OBJECT:UnderSynkKollFo(TRUE) = TRUE THEN DO:
               
         IF gruppkod = 0 THEN DO:
            IF THIS-OBJECT:andkonstvaltemp3TTh:AVAILABLE THEN DO:
               IF THIS-OBJECT:andkonstvaltemp5TTh:AVAILABLE THEN kvalkodvar5 = THIS-OBJECT:andkonstvaltemp5TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE.
               ELSE kvalkodvar5 = ?.            
               IF THIS-OBJECT:andkonstvaltemp4TTh:AVAILABLE THEN kvalkodvar4 = THIS-OBJECT:andkonstvaltemp4TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE.
               ELSE kvalkodvar4 = ?.
               
               THIS-OBJECT:ControlDataDb:andbrwb3(INPUT THIS-OBJECT:andkonstvaltemp3TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE).
               THIS-OBJECT:GridB5:GuruReOpen().
               IF kvalkodvar5 = ? THEN DO:
                  THIS-OBJECT:andkonstvaltemp5TTh:BUFFER-RELEASE().
                  THIS-OBJECT:GridB5:GuruAvmarkeraTot().
               END.
               ELSE DO:
                  THIS-OBJECT:andkonstvaltemp5TTh:FIND-FIRST("WHERE KVALKOD = '" + kvalkodvar5 + "'" ) NO-ERROR.
                  IF THIS-OBJECT:andkonstvaltemp5TTh:AVAILABLE THEN THIS-OBJECT:GridB5:GuruRepositionto(THIS-OBJECT:andkonstvaltemp5TTh:ROWID).
                  ELSE kvalkodvar5 = ?.
                  IF kvalkodvar5 = ? THEN DO:
                     THIS-OBJECT:andkonstvaltemp5TTh:BUFFER-RELEASE().
                     THIS-OBJECT:GridB5:GuruAvmarkeraTot().
                  END.
               END.            
              /* THIS-OBJECT:GridB4:GuruReOpen().
               IF kvalkodvar4 = ? THEN DO:
                  THIS-OBJECT:andkonstvaltemp4TTh:BUFFER-RELEASE().
                  THIS-OBJECT:GridB4:GuruAvmarkeraTot().
               END.
               ELSE DO:
                  THIS-OBJECT:andkonstvaltemp4TTh:FIND-FIRST("WHERE KVALKOD = '" + kvalkodvar4 + "'" ) NO-ERROR.
                  IF THIS-OBJECT:andkonstvaltemp4TTh:AVAILABLE THEN THIS-OBJECT:GridB4:GuruRepositionto(THIS-OBJECT:andkonstvaltemp4TTh:ROWID).
                  ELSE kvalkodvar4 = ?.
                  IF kvalkodvar4 = ? THEN DO:
                     THIS-OBJECT:andkonstvaltemp4TTh:BUFFER-RELEASE().
                     THIS-OBJECT:GridB4:GuruAvmarkeraTot().
                  END.
               END.            
               
               THIS-OBJECT:andkonstvaltemp2TTh:BUFFER-RELEASE().
               THIS-OBJECT:GridB2:GuruAvmarkeraTot().*/
            END. 
         END.
         ELSE DO:   
            IF THIS-OBJECT:andkonstvaltemp3TTh:AVAILABLE THEN DO:
               IF THIS-OBJECT:andkonstvaltemp5TTh:AVAILABLE THEN kvalkodvar5 = THIS-OBJECT:andkonstvaltemp5TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE.
               ELSE kvalkodvar5 = ?.            
               IF THIS-OBJECT:andkonstvaltemp4TTh:AVAILABLE THEN kvalkodvar4 = THIS-OBJECT:andkonstvaltemp4TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE.
               ELSE kvalkodvar4 = ?.
               
               THIS-OBJECT:ControlDataDb:andbrwb3(INPUT THIS-OBJECT:andkonstvaltemp3TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE).
               THIS-OBJECT:GridB5:GuruReOpen().
               IF kvalkodvar5 = ? THEN DO:
                  THIS-OBJECT:andkonstvaltemp5TTh:BUFFER-RELEASE().
                  THIS-OBJECT:GridB5:GuruAvmarkeraTot().
               END.
               ELSE DO:
                  THIS-OBJECT:andkonstvaltemp5TTh:FIND-FIRST("WHERE KVALKOD = '" + kvalkodvar5 + "'" ) NO-ERROR.
                  IF THIS-OBJECT:andkonstvaltemp5TTh:AVAILABLE THEN THIS-OBJECT:GridB5:GuruRepositionto(THIS-OBJECT:andkonstvaltemp5TTh:ROWID).
                  ELSE kvalkodvar5 = ?.
                  IF kvalkodvar5 = ? THEN DO:
                     THIS-OBJECT:andkonstvaltemp5TTh:BUFFER-RELEASE().
                     THIS-OBJECT:GridB5:GuruAvmarkeraTot().
                  END.
               END.            
               THIS-OBJECT:GridB4:GuruReOpen().
               IF kvalkodvar4 = ? OR kvalkodvar4 = "" THEN DO:
                  THIS-OBJECT:andkonstvaltemp4TTh:BUFFER-RELEASE().
                  THIS-OBJECT:GridB4:GuruAvmarkeraTot().
               END.
               ELSE DO:
                  THIS-OBJECT:andkonstvaltemp4TTh:FIND-FIRST("WHERE KVALKOD = '" + kvalkodvar4 + "'" ) NO-ERROR.
                  IF THIS-OBJECT:andkonstvaltemp4TTh:AVAILABLE THEN THIS-OBJECT:GridB4:GuruRepositionto(THIS-OBJECT:andkonstvaltemp4TTh:ROWID).
                  ELSE kvalkodvar4 = ?.
                  IF kvalkodvar4 = ? OR kvalkodvar4 = "" THEN DO:
                     THIS-OBJECT:andkonstvaltemp4TTh:BUFFER-RELEASE().
                     THIS-OBJECT:GridB4:GuruAvmarkeraTot().
                  END.
               END.            
               
               THIS-OBJECT:andkonstvaltemp2TTh:BUFFER-RELEASE().
               THIS-OBJECT:GridB2:GuruAvmarkeraTot().
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
	METHOD PRIVATE VOID GridB5_AfterSelectChange( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.AfterSelectChangeEventArgs ):
		DEFINE VARIABLE kvalkodvar2 AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kvalkodvar3 AS CHARACTER NO-UNDO.
      
      IF gruppkod = 0 THEN RETURN.
      IF THIS-OBJECT:UnderSynkKoll(TRUE) = TRUE THEN DO:
         IF THIS-OBJECT:andkonstvaltemp5TTh:AVAILABLE THEN DO:
            IF THIS-OBJECT:andkonstvaltemp2TTh:AVAILABLE THEN kvalkodvar2 = THIS-OBJECT:andkonstvaltemp2TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE.
            ELSE kvalkodvar2 = ?.
            IF THIS-OBJECT:andkonstvaltemp3TTh:AVAILABLE THEN kvalkodvar3 = THIS-OBJECT:andkonstvaltemp3TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE.
            ELSE kvalkodvar3 = ?. 
            THIS-OBJECT:ControlDataDb:andbrwb5(INPUT THIS-OBJECT:andkonstvaltemp5TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE).
            THIS-OBJECT:GridB2:GuruReOpen().
            IF kvalkodvar2 = ? THEN DO:
               THIS-OBJECT:andkonstvaltemp2TTh:BUFFER-RELEASE().
               THIS-OBJECT:GridB2:GuruAvmarkeraTot().
            END.
            ELSE DO:
               THIS-OBJECT:andkonstvaltemp2TTh:FIND-FIRST("WHERE KVALKOD = '" + kvalkodvar2 + "'" ) NO-ERROR.
               IF THIS-OBJECT:andkonstvaltemp2TTh:AVAILABLE THEN THIS-OBJECT:GridB2:GuruRepositionto(THIS-OBJECT:andkonstvaltemp2TTh:ROWID).
               ELSE kvalkodvar2 = ?.
            END.
            THIS-OBJECT:GridB3:GuruReOpen().
            IF kvalkodvar3 = ? THEN DO:
               THIS-OBJECT:andkonstvaltemp3TTh:BUFFER-RELEASE().
               THIS-OBJECT:GridB3:GuruAvmarkeraTot().
            END.     
            ELSE DO:
               THIS-OBJECT:andkonstvaltemp3TTh:FIND-FIRST("WHERE KVALKOD = '" + kvalkodvar3 + "'" ) NO-ERROR.
               IF THIS-OBJECT:andkonstvaltemp3TTh:AVAILABLE THEN THIS-OBJECT:GridB3:GuruRepositionto(THIS-OBJECT:andkonstvaltemp3TTh:ROWID).
               ELSE kvalkodvar3 = ?.
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
	METHOD PRIVATE VOID KabelAnd_Shown( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   IF gruppkod = 0 THEN.
      ELSE DO: 
         
         /*TABELLERNA ÄR FLYTTADE B2 TILL B5 B4 TILL B2 B5 TILL B4*/
         IF THIS-OBJECT:UnderSynkKollNat(TRUE) = TRUE THEN DO:
            /*ONEVATT*/
            IF Guru.Konstanter:globforetag = "GRAN" AND gruppkod = 57 THEN.
            ELSE DO:
               THIS-OBJECT:splitContainerB2B5:Panel1:Controls:Add(THIS-OBJECT:GridB5).   /*KABLAR*/        
               THIS-OBJECT:splitContainerB1B4:Panel2:Controls:Add(THIS-OBJECT:GridB2).   /*HSP*/
               THIS-OBJECT:splitContainerB2B5:Panel2:Controls:Add(THIS-OBJECT:GridB4).   /*SÄK*/
           END.            
         END. 
      END.
		
		IF synkkoll = TRUE THEN DO:
		   THIS-OBJECT:StartSync(). 
         IF GridRwid[2] NE ? THEN THIS-OBJECT:GridB2:GuruRepositionto(GridRwid[2]).
         ELSE THIS-OBJECT:GridB2:GuruAvmarkeraTot().
         IF GridRwid[3] NE ? THEN THIS-OBJECT:GridB3:GuruRepositionto(GridRwid[3]).
         ELSE THIS-OBJECT:GridB3:GuruAvmarkeraTot().
         IF GridRwid[4] NE ? THEN THIS-OBJECT:GridB4:GuruRepositionto(GridRwid[4]).
         ELSE THIS-OBJECT:GridB4:GuruAvmarkeraTot().
         IF GridRwid[5] NE ? THEN THIS-OBJECT:GridB5:GuruRepositionto(GridRwid[5]).
         ELSE THIS-OBJECT:GridB5:GuruAvmarkeraTot().
         IF GridRwid[6] NE ? THEN THIS-OBJECT:GridB6:GuruRepositionto(GridRwid[6]).
         ELSE THIS-OBJECT:GridB6:GuruAvmarkeraTot().
         synkkoll = FALSE.
      END.
      IF gruppkod = 0 THEN DO:
         IF GridRwid[2] NE ? THEN THIS-OBJECT:GridB2_AfterSelectChange(?,?).
         IF GridRwid[3] NE ? THEN THIS-OBJECT:GridB3_AfterSelectChange(?,?).
      END.    
      ELSE DO:
         IF GridRwid[5] NE ? THEN THIS-OBJECT:GridB5_AfterSelectChange(?,?).
         IF THIS-OBJECT:UnderSynkKollNat(TRUE) = TRUE THEN DO:
            THIS-OBJECT:StartSync2().
         END.   
         
         IF GridRwid[2] NE ? THEN THIS-OBJECT:GridB2:GuruRepositionto(GridRwid[2]).
         ELSE THIS-OBJECT:GridB2:GuruAvmarkeraTot().
         IF GridRwid[3] NE ? THEN THIS-OBJECT:GridB3:GuruRepositionto(GridRwid[3]).
         ELSE THIS-OBJECT:GridB3:GuruAvmarkeraTot().
         IF GridRwid[4] NE ? THEN THIS-OBJECT:GridB4:GuruRepositionto(GridRwid[4]).
         ELSE THIS-OBJECT:GridB4:GuruAvmarkeraTot().
         IF GridRwid[6] NE ? THEN THIS-OBJECT:GridB6:GuruRepositionto(GridRwid[6]).
         ELSE THIS-OBJECT:GridB6:GuruAvmarkeraTot(). 
      END. 
      IF GridRwid[2] = ? THEN THIS-OBJECT:GridB2:GuruAvmarkeraTot().
      IF GridRwid[3] = ? THEN THIS-OBJECT:GridB3:GuruAvmarkeraTot().
      IF GridRwid[4] = ? THEN THIS-OBJECT:GridB4:GuruAvmarkeraTot().
      IF GridRwid[5] = ? THEN THIS-OBJECT:GridB5:GuruAvmarkeraTot().
      IF GridRwid[6] = ? THEN THIS-OBJECT:GridB6:GuruAvmarkeraTot().
     
      RETURN.

	END METHOD.


	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID RemoveButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ButtonBort_Click(?,?).
		RETURN.

	END METHOD.

   METHOD PUBLIC LOGICAL UnderSynkKollNat(synk AS LOGICAL):
       
      {UNDERSYNKKOLL.I}
   END METHOD.
   METHOD PUBLIC LOGICAL UnderSynkKoll(synk AS LOGICAL):
       IF synkkoll = TRUE  THEN RETURN FALSE.
      {UNDERSYNKKOLL.I}
   END METHOD.
   METHOD PUBLIC LOGICAL UnderSynkKollFo(synk AS LOGICAL):
      IF synkkoll = TRUE  THEN RETURN FALSE.
      IF  Guru.Konstanter:globforetag = "FORS" THEN RETURN synk.
      ELSE IF  Guru.Konstanter:globforetag = "KRIN" THEN RETURN synk.
      ELSE IF  Guru.Konstanter:varforetypval[64] = 1 THEN RETURN synk.
      
      ELSE RETURN FALSE.
   END METHOD.
   
   
	
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridKabAnd_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString(). 
      kolvarde = e:Cell:VALUE:ToString().
      THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
      THIS-OBJECT:Fri_tempTTh:FIND-FIRST("WHERE NUM = " + STRING(THIS-OBJECT:ControlDataDb:BigNumvar) + 
      " AND KABNR = " + STRING(THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE)) NO-ERROR.
      IF THIS-OBJECT:Fri_tempTTh:AVAILABLE THEN.
      ELSE DO:
         
         THIS-OBJECT:Fri_tempTTh:BUFFER-CREATE(). 
         THIS-OBJECT:Fri_tempTTh:BUFFER-FIELD("NUM"):BUFFER-VALUE = THIS-OBJECT:ControlDataDb:BigNumvar.
         THIS-OBJECT:Fri_tempTTh:BUFFER-FIELD("KABNR"):BUFFER-VALUE = THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE.
         THIS-OBJECT:Fri_tempTTh:BUFFER-FIELD("VARSKAPAD"):BUFFER-VALUE = TRUE.   
         THIS-OBJECT:Fri_tempTTh:BUFFER-FIELD("ARTAL"):BUFFER-VALUE = YEAR(TODAY).
      END.
      THIS-OBJECT:Fri_tempTTh:BUFFER-FIELD("KABADR"):BUFFER-VALUE = THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("KABELADRESS"):BUFFER-VALUE.     
      IF beforekolnamn = "F2" OR beforekolnamn = "F3" OR beforekolnamn = "F4" OR beforekolnamn = "F5" OR beforekolnamn = "F6" THEN  THIS-OBJECT:ControlDataDb:BytKabelDelTill().    
      IF gruppkod = 0 THEN DO:
         IF  beforekolnamn = "F5" THEN THIS-OBJECT:Fri_tempTTh:BUFFER-FIELD("SAKR"):BUFFER-VALUE = 0.
      END.
      ELSE DO:
         IF beforekolnamn = "F4" THEN THIS-OBJECT:Fri_tempTTh:BUFFER-FIELD("SAKR"):BUFFER-VALUE = 0.
      END.
      RETURN.

	END METHOD.

	
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridKabAnd_BeforeCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.BeforeCellUpdateEventArgs ):
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
		beforekolnamn = e:Cell:Column:ToString().        
      beforekolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:GridKabAnd:Guruegenskap:getRubrik(beforekolnamn):Readonly = TRUE THEN DO:
         e:Cancel = TRUE.
         RETURN.
      END.   
      
      THIS-OBJECT:KskKon_valTTh:FIND-FIRST("WHERE TTRECID = " + STRING(THIS-OBJECT:GridKabAnd:ActiveRow:Cells["TTRECID"]:VALUE ), NO-LOCK) NO-ERROR.
      IF THIS-OBJECT:ControlDataDb:InkopKoll() = TRUE THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(Guru.konstanter:globalroot:LanguageManager:GetStringAsMessage(194), "").
         e:Cancel = TRUE.
         RETURN. 
      END. 
      IF THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE = 99  THEN DO:
         e:Cancel = TRUE.
         RETURN.  
      END.
      
      IF beforekolnamn = "F2" OR beforekolnamn = "F3" OR beforekolnamn = "F4" OR beforekolnamn = "F5" OR beforekolnamn = "F6" THEN  THIS-OBJECT:ControlDataDb:BytKabelDelFran().  
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID KabelAnd_FormClosing( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.FormClosingEventArgs ):
		THIS-OBJECT:ButtonOK:Select().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID KabelAnd_Load( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	  
		RETURN.

	END METHOD.

   METHOD PRIVATE VOID okButton_Click (sender AS System.Object, e AS System.EventArgs):
		THIS-OBJECT:DialogResult = System.Windows.Forms.DialogResult:Ok.
		THIS-OBJECT:Close ( ). 
	END METHOD.

	
	METHOD PUBLIC VOID CreateForm():
	   THIS-OBJECT:Text = "Komplettera " + rubrikvar.
      THIS-OBJECT:CreateGrids().
      THIS-OBJECT:CreateGridFunctions().
      THIS-OBJECT:KskKon_valTTh:FIND-FIRST("WHERE  ") NO-ERROR.
      IF THIS-OBJECT:KskKon_valTTh:AVAILABLE THEN DO:
         THIS-OBJECT:ControlDataDb:UppMtrl().
      END.
      THIS-OBJECT:splitContainerNyBort:Panel2Collapsed = TRUE.
      
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridFunctions():
      THIS-OBJECT:ButtonNy:GuruKoppla(THIS-OBJECT:GridB1,THIS-OBJECT:GridKabAnd).
      THIS-OBJECT:ButtonNy:GuruDublett = TRUE.
      THIS-OBJECT:ButtonBort:GuruKoppla(THIS-OBJECT:GridKabAnd, THIS-OBJECT:GridB1).
      THIS-OBJECT:ButtonBort:GuruRemoveFrom = TRUE.
      
   END METHOD.
   
   METHOD PUBLIC VOID CreateGrids():
      THIS-OBJECT:CreateGridB().
      THIS-OBJECT:CreateGridKonMtrl().
      synkkoll = TRUE.
            
   END METHOD.
   METHOD PUBLIC VOID StartSync():
      /*öppnar allt*/
      
      IF THIS-OBJECT:KskKon_valTTh:AVAILABLE THEN DO:
         
         THIS-OBJECT:StartSync2().
         IF THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F5"):BUFFER-VALUE NE "" THEN DO:
            THIS-OBJECT:andkonstvaltemp5TTh:FIND-FIRST("WHERE KVALKOD = '" + THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F5"):BUFFER-VALUE + "'" ) NO-ERROR.
            IF THIS-OBJECT:andkonstvaltemp5TTh:AVAILABLE THEN DO:
               GridRwid[5] = andkonstvaltemp5TTh:ROWID.         
            END.
         END.
         ELSE THIS-OBJECT:GridB5:GuruAvmarkeraTot(). 
      END.
      
   END METHOD.
   METHOD PUBLIC VOID StartSync2():
      /*öppnar allt*/
      
      IF THIS-OBJECT:KskKon_valTTh:AVAILABLE THEN DO:
         IF THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F2"):BUFFER-VALUE NE "" THEN DO:
            THIS-OBJECT:andkonstvaltemp2TTh:FIND-FIRST("WHERE KVALKOD = '" + THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F2"):BUFFER-VALUE + "'" ) NO-ERROR.
            IF THIS-OBJECT:andkonstvaltemp2TTh:AVAILABLE THEN DO:
               GridRwid[2] = andkonstvaltemp2TTh:ROWID.              
            END.
         END.
         ELSE THIS-OBJECT:GridB2:GuruAvmarkeraTot().
         IF THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F3"):BUFFER-VALUE NE "" THEN DO:
            THIS-OBJECT:andkonstvaltemp3TTh:FIND-FIRST("WHERE KVALKOD = '" + THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F3"):BUFFER-VALUE + "'" ) NO-ERROR.
            IF THIS-OBJECT:andkonstvaltemp3TTh:AVAILABLE THEN DO:
               GridRwid[3] = andkonstvaltemp3TTh:ROWID.             
            END.
         END.
         ELSE THIS-OBJECT:GridB3:GuruAvmarkeraTot().
         IF THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F4"):BUFFER-VALUE NE "" THEN DO:
            THIS-OBJECT:andkonstvaltemp4TTh:FIND-FIRST("WHERE KVALKOD = '" + THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F4"):BUFFER-VALUE + "'" ) NO-ERROR.
            IF THIS-OBJECT:andkonstvaltemp4TTh:AVAILABLE THEN DO:
               GridRwid[4] = andkonstvaltemp4TTh:ROWID.               
            END.
         END.
         ELSE THIS-OBJECT:GridB4:GuruAvmarkeraTot().
         
         IF THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F6"):BUFFER-VALUE NE "" THEN DO:
            THIS-OBJECT:andkonstvaltemp6TTh:FIND-FIRST("WHERE KVALKOD = '" + THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("F6"):BUFFER-VALUE + "'" ) NO-ERROR.
            IF THIS-OBJECT:andkonstvaltemp6TTh:AVAILABLE THEN DO:
               GridRwid[6] = andkonstvaltemp6TTh:ROWID.               
            END.
         END.
         ELSE THIS-OBJECT:GridB6:GuruAvmarkeraTot().
        
      END.
      
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridKonMtrl():
      {GridRubrikListaVarExtent.i}
      
      rubrikergrid[1] = NEW Controls.GridRubrik("ENR",Guru.Konstanter:genk,FALSE,55).      
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,310).
      rubrikergrid[3] = NEW Controls.GridRubrik("ANTAL","Antal",TRUE,50).
      rubrikergrid[4] = NEW Controls.GridRubrik("LEVKOD","Lev",TRUE,50).
      
      THIS-OBJECT:GridKonMtrl:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 4, INPUT THIS-OBJECT:Upp_mtrlTTh)). 
     /* THIS-OBJECT:GridKonMtrl:GuruReOpen().*/
      THIS-OBJECT:GridKonMtrl:SetTitle("Visa Materiel enligt upplägget").
      THIS-OBJECT:GridKonMtrl:VisaViaInternetMenuItemAddAdd().
      
      IF Guru.Konstanter:varforetypval[29] = 1 THEN DO:
        
         THIS-OBJECT:GridKonMtrl:GuruGridFargM(TRUE).             
         THIS-OBJECT:GridKonMtrl:GuruAddConditionalColor("KUND","true",?, Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[28]),Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[38])).
         
      END. 
     
      THIS-OBJECT:GridBerMtrl:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 4, INPUT THIS-OBJECT:List_mtrlTTh)). 
   /*   THIS-OBJECT:GridBerMtrl:GuruReOpen().*/
      THIS-OBJECT:GridBerMtrl:SetTitle("Materiel för vald rad").
      THIS-OBJECT:GridBerMtrl:VisaViaInternetMenuItemAddAdd().
      
     
      IF Guru.Konstanter:varforetypval[29] = 1 THEN DO:
        
         THIS-OBJECT:GridBerMtrl:GuruGridFargM(TRUE).             
         THIS-OBJECT:GridBerMtrl:GuruAddConditionalColor("KUND","true",?, Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[28]),Guru.StaticMethods:DotNetColor(Guru.Konstanter:varforetypval[38])).
         
      END. 
     
      
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridB():
      THIS-OBJECT:BbenamntempTTh:FIND-FIRST("WHERE  ") NO-ERROR.
      IF bbenamntempTTh:AVAILABLE THEN.
      ELSE RETURN.
        
      {GridRubrikListaVarExtent.i}
    
      rubrikergrid[1] = NEW Controls.GridRubrik("KTYPKOD","Kod",TRUE,55).      
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,280).
      THIS-OBJECT:GridB1:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 2, INPUT THIS-OBJECT:KonstTTh)). 
      THIS-OBJECT:GridB1:GuruReOpen().
      
      IF THIS-OBJECT:BbenamntempTTh:AVAILABLE THEN DO:
         THIS-OBJECT:GridB1:SetTitle(THIS-OBJECT:BbenamntempTTh:BUFFER-FIELD("B1"):BUFFER-VALUE).
      END.
      THIS-OBJECT:GridB1:ExtramenuItemAdd("Avmarkera alla").
      THIS-OBJECT:GridB1:ExtraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:AvmarkeraB1).
      THIS-OBJECT:GridB1:ExtramenuItemAdd("Visa Materiel enligt upplägget"). 
      THIS-OBJECT:GridB1:ExtraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:MtrUpp1).
     
      rubrikergrid[1] = NEW Controls.GridRubrik("TRIMKVALKOD","Kod",TRUE,80).
      THIS-OBJECT:GridB2:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 2, INPUT THIS-OBJECT:andkonstvaltemp2TTh)).
      THIS-OBJECT:GridB2:GuruOrderby("BY ORDNING ").
      THIS-OBJECT:GridB2:SetTitle(THIS-OBJECT:BbenamntempTTh:BUFFER-FIELD("B2"):BUFFER-VALUE). 
      
      THIS-OBJECT:GridB2:ExtramenuItemAdd("Avmarkera"). 
      THIS-OBJECT:GridB2:ExtraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:AvmarkeraB2).
      THIS-OBJECT:GridB2:ExtramenuItemAdd("Visa Materiel enligt upplägget"). 
      THIS-OBJECT:GridB2:ExtraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:MtrUpp2).
      THIS-OBJECT:GridB2:GuruSortable = FALSE.
            
      THIS-OBJECT:GridB3:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 2, INPUT THIS-OBJECT:andkonstvaltemp3TTh)).
      THIS-OBJECT:GridB3:GuruOrderby("BY ORDNING ").
      THIS-OBJECT:GridB3:SetTitle(THIS-OBJECT:BbenamntempTTh:BUFFER-FIELD("B3"):BUFFER-VALUE). 
      
      THIS-OBJECT:GridB3:ExtramenuItemAdd("Avmarkera").
      THIS-OBJECT:GridB3:ExtraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:AvmarkeraB3).
      THIS-OBJECT:GridB3:ExtramenuItemAdd("Visa Materiel enligt upplägget"). 
      THIS-OBJECT:GridB3:ExtraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:MtrUpp3).
     
      THIS-OBJECT:GridB3:GuruSortable = FALSE.
      
      THIS-OBJECT:GridB4:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 2, INPUT THIS-OBJECT:andkonstvaltemp4TTh)).
      THIS-OBJECT:GridB4:GuruOrderby("BY ORDNING ").
      THIS-OBJECT:GridB4:SetTitle(THIS-OBJECT:BbenamntempTTh:BUFFER-FIELD("B4"):BUFFER-VALUE). 
      THIS-OBJECT:GridB4:ExtramenuItemAdd("Avmarkera").
      THIS-OBJECT:GridB4:ExtraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:AvmarkeraB4).
      THIS-OBJECT:GridB4:ExtramenuItemAdd("Visa Materiel enligt upplägget").
      THIS-OBJECT:GridB4:ExtraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:MtrUpp4).
      THIS-OBJECT:GridB4:GuruSortable = FALSE.
      
      THIS-OBJECT:GridB5:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 2, INPUT THIS-OBJECT:andkonstvaltemp5TTh)).
      THIS-OBJECT:GridB5:GuruOrderby("BY ORDNING ").
      THIS-OBJECT:GridB5:SetTitle(THIS-OBJECT:BbenamntempTTh:BUFFER-FIELD("B5"):BUFFER-VALUE). 
      
      THIS-OBJECT:GridB5:ExtramenuItemAdd("Avmarkera").
      THIS-OBJECT:GridB5:ExtraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:AvmarkeraB5).
      THIS-OBJECT:GridB5:ExtramenuItemAdd("Visa Materiel enligt upplägget"). 
      THIS-OBJECT:GridB5:ExtraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:MtrUpp5).
      THIS-OBJECT:GridB5:GuruSortable = FALSE.
      
      THIS-OBJECT:GridB6:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 2, INPUT THIS-OBJECT:andkonstvaltemp6TTh)).
      THIS-OBJECT:GridB6:GuruOrderby("BY ORDNING ").
      THIS-OBJECT:GridB6:SetTitle(THIS-OBJECT:BbenamntempTTh:BUFFER-FIELD("B6"):BUFFER-VALUE). 
      THIS-OBJECT:GridB6:ExtramenuItemAdd("Avmarkera").
      THIS-OBJECT:GridB6:ExtraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:AvmarkeraB6).
      THIS-OBJECT:GridB6:ExtramenuItemAdd("Visa Materiel enligt upplägget"). 
      THIS-OBJECT:GridB6:ExtraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:MtrUpp6).
      THIS-OBJECT:GridB6:GuruSortable = FALSE.
      
      /*
      rubrikergrid[1] = NEW Controls.GridRubrik("F1",STRING(THIS-OBJECT:BbenamntempTTh:BUFFER-FIELD("B1"):BUFFER-VALUE),TRUE,100).
      rubrikergrid[2] = NEW Controls.GridRubrik("F2",STRING(THIS-OBJECT:BbenamntempTTh:BUFFER-FIELD("B2"):BUFFER-VALUE),FALSE,110).
      rubrikergrid[3] = NEW Controls.GridRubrik("F3",STRING(THIS-OBJECT:BbenamntempTTh:BUFFER-FIELD("B3"):BUFFER-VALUE),FALSE,110).
      rubrikergrid[4] = NEW Controls.GridRubrik("F4",STRING(THIS-OBJECT:BbenamntempTTh:BUFFER-FIELD("B4"):BUFFER-VALUE),FALSE,110).
      rubrikergrid[5] = NEW Controls.GridRubrik("F5",STRING(THIS-OBJECT:BbenamntempTTh:BUFFER-FIELD("B5"):BUFFER-VALUE),FALSE,110).
      rubrikergrid[6] = NEW Controls.GridRubrik("F6",STRING(THIS-OBJECT:BbenamntempTTh:BUFFER-FIELD("B6"):BUFFER-VALUE),FALSE,110).
      */
      rubrikergrid[1] = NEW Controls.GridRubrik("F1",STRING(THIS-OBJECT:ControlDataDb:b1var),TRUE,100).
      rubrikergrid[2] = NEW Controls.GridRubrik("F2",STRING(THIS-OBJECT:ControlDataDb:b2var),FALSE,110).
      rubrikergrid[3] = NEW Controls.GridRubrik("F3",STRING(THIS-OBJECT:ControlDataDb:b3var),FALSE,110).
      rubrikergrid[4] = NEW Controls.GridRubrik("F4",STRING(THIS-OBJECT:ControlDataDb:b4var),FALSE,110).
      rubrikergrid[5] = NEW Controls.GridRubrik("F5",STRING(THIS-OBJECT:ControlDataDb:b5var),FALSE,110).
      rubrikergrid[6] = NEW Controls.GridRubrik("F6",STRING(THIS-OBJECT:ControlDataDb:b6var),FALSE,110).
      rubrikergrid[7] = NEW Controls.GridRubrik("KABELADRESS","Kabeladress",FALSE,300).
      
      rubrikergrid[8] = NEW Controls.GridRubrik("SKAPNUM","SKAPNUM",TRUE,50).      
      rubrikergrid[9] = NEW Controls.GridRubrik("KSKAP","KSKAP",TRUE,50).
      
      THIS-OBJECT:GridKabAnd:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 7, INPUT THIS-OBJECT:KskKon_valTTh)).
      THIS-OBJECT:GridKabAnd:GuruOrderby("BY SKAPNUM ").
      THIS-OBJECT:GridKabAnd:GuruSortable = FALSE.    
      THIS-OBJECT:GridKabAnd:SetTitle(rubrikvar). 
      THIS-OBJECT:GridKabAnd:ExtramenuItemAdd("Visa Materiel för vald rad"). 
      THIS-OBJECT:GridKabAnd:ExtraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:BerMtrUpp).
      /*THIS-OBJECT:GridKabAnd:ExtramenuItemAdd("Import byte kabelskåp från mall"). 
      THIS-OBJECT:GridKabAnd:ExtraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:KskImpMall).*/
      comboList[2] = NEW Infragistics.Win.ValueList().
      comboList[3] = NEW Infragistics.Win.ValueList().
      comboList[4] = NEW Infragistics.Win.ValueList().
      comboList[5] = NEW Infragistics.Win.ValueList().
      comboList[6] = NEW Infragistics.Win.ValueList().
      comboList[2]:ValueListItems:Add("").
      comboList[3]:ValueListItems:Add("").
      comboList[4]:ValueListItems:Add("").
      comboList[5]:ValueListItems:Add("").
      comboList[6]:ValueListItems:Add("").
      queryvar =  "FOR EACH " + THIS-OBJECT:KonstvaltempTTh:TABLE + " WHERE BB = '" + THIS-OBJECT:ControlDataDb:b2var + "'" + " BY ORDNING ". 
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KonstvaltempTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         comboList[2]:ValueListItems:Add(THIS-OBJECT:KonstvaltempTTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE).
         qH:GET-NEXT().
      END.
      queryvar =  "FOR EACH " + THIS-OBJECT:KonstvaltempTTh:TABLE + " WHERE BB = '" + THIS-OBJECT:ControlDataDb:b3var + "'" + " BY ORDNING ". 
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KonstvaltempTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         comboList[3]:ValueListItems:Add(THIS-OBJECT:KonstvaltempTTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE).
         qH:GET-NEXT().
      END.
      queryvar =  "FOR EACH " + THIS-OBJECT:KonstvaltempTTh:TABLE + " WHERE BB = '" + THIS-OBJECT:ControlDataDb:b4var + "'" + " BY ORDNING ". 
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KonstvaltempTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         comboList[4]:ValueListItems:Add(THIS-OBJECT:KonstvaltempTTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE).
         qH:GET-NEXT().
      END.
      queryvar =  "FOR EACH " + THIS-OBJECT:KonstvaltempTTh:TABLE + " WHERE BB = '" + THIS-OBJECT:ControlDataDb:b5var + "'" + " BY ORDNING ". 
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KonstvaltempTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         comboList[5]:ValueListItems:Add(THIS-OBJECT:KonstvaltempTTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE).
         qH:GET-NEXT().
      END.
      queryvar =  "FOR EACH " + THIS-OBJECT:KonstvaltempTTh:TABLE + " WHERE BB = '" + THIS-OBJECT:ControlDataDb:b6var + "'" + " BY ORDNING ". 
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:KonstvaltempTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         comboList[6]:ValueListItems:Add(THIS-OBJECT:KonstvaltempTTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE).
         qH:GET-NEXT().
      END.
       
      THIS-OBJECT:GridKabAnd:GuruComboCellRefill("F2",comboList[2]).
      THIS-OBJECT:GridKabAnd:GuruComboCellRefill("F3",comboList[3]).
      THIS-OBJECT:GridKabAnd:GuruComboCellRefill("F4",comboList[4]).
      THIS-OBJECT:GridKabAnd:GuruComboCellRefill("F5",comboList[5]).
      THIS-OBJECT:GridKabAnd:GuruComboCellRefill("F6",comboList[6]).
      
      THIS-OBJECT:GridKabAnd:GuruFirstrow().
      
   END METHOD. 
    
	METHOD PRIVATE VOID InitializeComponent ( ):

		/* NOTE: The following method is automatically generated.
		   
		   We strongly suggest that the contents of this method only be modified using the 
		   Visual Designer to avoid any incompatible modifications.
		   
		   Modifying the contents of this method using a code editor will invalidate any support for this file. */
		
      THIS-OBJECT:components = NEW System.ComponentModel.Container().
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
      resources = NEW Progress.Util.ResourceManager("Modules.Beredning.KabelAnd").
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
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance111 AS Infragistics.Win.Appearance NO-UNDO.
      appearance111 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance112 AS Infragistics.Win.Appearance NO-UNDO.
      appearance112 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance113 AS Infragistics.Win.Appearance NO-UNDO.
      appearance113 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance114 AS Infragistics.Win.Appearance NO-UNDO.
      appearance114 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE tableDesc1 AS Progress.Data.TableDesc NO-UNDO.
      tableDesc1 = NEW Progress.Data.TableDesc("andrakon_val").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance115 AS Infragistics.Win.Appearance NO-UNDO.
      appearance115 = NEW Infragistics.Win.Appearance().
      THIS-OBJECT:splitContainerKabelAnd = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerValdaKon = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridKabAnd = NEW Controls.Grid().
      THIS-OBJECT:ultraTextEditorOrdning = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      THIS-OBJECT:ButtonNer = NEW Controls.ButtonUpDown().
      THIS-OBJECT:ButtonUpp = NEW Controls.ButtonUpDown().
      THIS-OBJECT:PopBerMtrlPanel = NEW Controls.PopPanel().
      THIS-OBJECT:SplitContainerPopBerMtrl = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridBerMtrl = NEW Controls.Grid().
      THIS-OBJECT:PopBerMtrlOK = NEW Infragistics.Win.Misc.UltraButton().
      THIS-OBJECT:splitContainerKonst = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerAddRemo = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerNyBort = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:AddRemoveStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:RemoveButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:AddButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ClearButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonNy = NEW Controls.ButtonOver().
      THIS-OBJECT:ButtonBort = NEW Controls.ButtonOver().
      THIS-OBJECT:splitContainerValjKonst = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContaineB1B2B4B5 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerB1B4 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridB1 = NEW Controls.Grid().
      THIS-OBJECT:GridB4 = NEW Controls.Grid().
      THIS-OBJECT:splitContainerB2B5 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridB2 = NEW Controls.Grid().
      THIS-OBJECT:GridB5 = NEW Controls.Grid().
      THIS-OBJECT:splitContainerB3B6 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridB3 = NEW Controls.Grid().
      THIS-OBJECT:GridB6 = NEW Controls.Grid().
      THIS-OBJECT:PopMtrlPanel = NEW Controls.PopPanel().
      THIS-OBJECT:splitContainerPopMtrlPanel = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridKonMtrl = NEW Controls.Grid().
      THIS-OBJECT:PopMtrlOK = NEW Infragistics.Win.Misc.UltraButton().
      THIS-OBJECT:bindingSource1 = NEW Progress.Data.BindingSource(THIS-OBJECT:components).
      THIS-OBJECT:ButtonOK = NEW Infragistics.Win.Misc.UltraButton().
      THIS-OBJECT:PopupMtrl = NEW Infragistics.Win.Misc.UltraPopupControlContainer(THIS-OBJECT:components).
      THIS-OBJECT:PopupBerMtrl = NEW Infragistics.Win.Misc.UltraPopupControlContainer(THIS-OBJECT:components).
      THIS-OBJECT:splitContainerStart = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerButtunOk = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:BTN_KALK = NEW Controls.Button().
      THIS-OBJECT:BTN_SKAP = NEW Controls.Button().
      THIS-OBJECT:BTN_MTRL = NEW Controls.Button().
      THIS-OBJECT:BTN_ID = NEW Controls.Button().
      CAST(THIS-OBJECT:splitContainerKabelAnd, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKabelAnd:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKabelAnd:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKabelAnd:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerValdaKon, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerValdaKon:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerValdaKon:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerValdaKon:SuspendLayout().
      CAST(THIS-OBJECT:GridKabAnd, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:ultraTextEditorOrdning, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:PopBerMtrlPanel:ClientArea:SuspendLayout().
      THIS-OBJECT:PopBerMtrlPanel:SuspendLayout().
      CAST(THIS-OBJECT:SplitContainerPopBerMtrl, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SplitContainerPopBerMtrl:Panel1:SuspendLayout().
      THIS-OBJECT:SplitContainerPopBerMtrl:Panel2:SuspendLayout().
      THIS-OBJECT:SplitContainerPopBerMtrl:SuspendLayout().
      CAST(THIS-OBJECT:GridBerMtrl, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerKonst, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerKonst:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKonst:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKonst:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerAddRemo, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerAddRemo:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerAddRemo:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerNyBort, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerNyBort:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerNyBort:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerNyBort:SuspendLayout().
      THIS-OBJECT:AddRemoveStrip:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerValjKonst, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerValjKonst:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerValjKonst:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerValjKonst:SuspendLayout().
      CAST(THIS-OBJECT:splitContaineB1B2B4B5, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContaineB1B2B4B5:Panel1:SuspendLayout().
      THIS-OBJECT:splitContaineB1B2B4B5:Panel2:SuspendLayout().
      THIS-OBJECT:splitContaineB1B2B4B5:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerB1B4, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerB1B4:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerB1B4:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerB1B4:SuspendLayout().
      CAST(THIS-OBJECT:GridB1, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridB4, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerB2B5, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerB2B5:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerB2B5:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerB2B5:SuspendLayout().
      CAST(THIS-OBJECT:GridB2, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridB5, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerB3B6, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerB3B6:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerB3B6:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerB3B6:SuspendLayout().
      CAST(THIS-OBJECT:GridB3, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:GridB6, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:PopMtrlPanel:ClientArea:SuspendLayout().
      THIS-OBJECT:PopMtrlPanel:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerPopMtrlPanel, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerPopMtrlPanel:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerPopMtrlPanel:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerPopMtrlPanel:SuspendLayout().
      CAST(THIS-OBJECT:GridKonMtrl, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerStart, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerStart:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerStart:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerStart:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerButtunOk, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerButtunOk:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerButtunOk:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerButtunOk:SuspendLayout().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainerKabelAnd */
      /*  */
      THIS-OBJECT:splitContainerKabelAnd:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKabelAnd:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKabelAnd:Name = "splitContainerKabelAnd".
      THIS-OBJECT:splitContainerKabelAnd:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerKabelAnd.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKabelAnd:Panel1:Controls:Add(THIS-OBJECT:splitContainerValdaKon).
      THIS-OBJECT:splitContainerKabelAnd:Panel1:Controls:Add(THIS-OBJECT:PopBerMtrlPanel).
      /*  */
      /* splitContainerKabelAnd.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKabelAnd:Panel2:Controls:Add(THIS-OBJECT:splitContainerKonst).
      THIS-OBJECT:splitContainerKabelAnd:Panel2:Controls:Add(THIS-OBJECT:PopMtrlPanel).
      THIS-OBJECT:splitContainerKabelAnd:Size = NEW System.Drawing.Size(1194, 603).
      THIS-OBJECT:splitContainerKabelAnd:SplitterDistance = 250.
      THIS-OBJECT:splitContainerKabelAnd:TabIndex = 0.
      /*  */
      /* splitContainerValdaKon */
      /*  */
      THIS-OBJECT:splitContainerValdaKon:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerValdaKon:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerValdaKon:Name = "splitContainerValdaKon".
      /*  */
      /* splitContainerValdaKon.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerValdaKon:Panel1:Controls:Add(THIS-OBJECT:GridKabAnd).
      /*  */
      /* splitContainerValdaKon.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerValdaKon:Panel2:Controls:Add(THIS-OBJECT:ultraTextEditorOrdning).
      THIS-OBJECT:splitContainerValdaKon:Panel2:Controls:Add(THIS-OBJECT:ButtonNer).
      THIS-OBJECT:splitContainerValdaKon:Panel2:Controls:Add(THIS-OBJECT:ButtonUpp).
      THIS-OBJECT:splitContainerValdaKon:Size = NEW System.Drawing.Size(1194, 250).
      THIS-OBJECT:splitContainerValdaKon:SplitterDistance = 975.
      THIS-OBJECT:splitContainerValdaKon:TabIndex = 0.
      /*  */
      /* GridKabAnd */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridKabAnd:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:GridKabAnd:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridKabAnd:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKabAnd:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKabAnd:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:GridKabAnd:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKabAnd:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:GridKabAnd:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridKabAnd:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridKabAnd:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridKabAnd:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridKabAnd:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:GridKabAnd:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridKabAnd:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKabAnd:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridKabAnd:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:GridKabAnd:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridKabAnd:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKabAnd:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:GridKabAnd:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:GridKabAnd:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridKabAnd:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridKabAnd:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:GridKabAnd:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridKabAnd:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:GridKabAnd:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridKabAnd:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridKabAnd:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridKabAnd:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridKabAnd:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridKabAnd:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridKabAnd:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridKabAnd:GuruContext = ?.
      THIS-OBJECT:GridKabAnd:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridKabAnd:Name = "GridKabAnd".
      THIS-OBJECT:GridKabAnd:Size = NEW System.Drawing.Size(975, 250).
      THIS-OBJECT:GridKabAnd:TabIndex = 0.
      THIS-OBJECT:GridKabAnd:Text = "grid1".
      THIS-OBJECT:GridKabAnd:AfterCellUpdate:Subscribe(THIS-OBJECT:GridKabAnd_AfterCellUpdate).

      THIS-OBJECT:GridKabAnd:BeforeCellUpdate:Subscribe(THIS-OBJECT:GridKabAnd_BeforeCellUpdate).
      /*  */
      /* ultraTextEditorOrdning */
      /*  */
      THIS-OBJECT:ultraTextEditorOrdning:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(15.75), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
      THIS-OBJECT:ultraTextEditorOrdning:Location = NEW System.Drawing.Point(31, 28).
      THIS-OBJECT:ultraTextEditorOrdning:Name = "ultraTextEditorOrdning".
      THIS-OBJECT:ultraTextEditorOrdning:ReadOnly = TRUE.
      THIS-OBJECT:ultraTextEditorOrdning:Size = NEW System.Drawing.Size(172, 34).
      THIS-OBJECT:ultraTextEditorOrdning:TabIndex = 2.
      THIS-OBJECT:ultraTextEditorOrdning:Text = "Ändra Ordning".
      /*  */
      /* ButtonNer */
      /*  */
      appearance13:Image = CAST(resources:GetObject("appearance13.Image"), System.Object).
      THIS-OBJECT:ButtonNer:Appearance = appearance13.
      THIS-OBJECT:ButtonNer:GuruTooltip = "Flytta ner vald kabel".
      THIS-OBJECT:ButtonNer:Location = NEW System.Drawing.Point(100, 99).
      THIS-OBJECT:ButtonNer:Name = "ButtonNer".
      THIS-OBJECT:ButtonNer:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:ButtonNer:TabIndex = 1.
      THIS-OBJECT:ButtonNer:Text = " ".
      THIS-OBJECT:ButtonNer:Click:Subscribe(THIS-OBJECT:ButtonNer_Click).
      /*  */
      /* ButtonUpp */
      /*  */
      appearance14:Image = CAST(resources:GetObject("appearance14.Image"), System.Object).
      THIS-OBJECT:ButtonUpp:Appearance = appearance14.
      THIS-OBJECT:ButtonUpp:GuruTooltip = "Flytta upp vald kabel ".
      THIS-OBJECT:ButtonUpp:Location = NEW System.Drawing.Point(100, 70).
      THIS-OBJECT:ButtonUpp:Name = "ButtonUpp".
      THIS-OBJECT:ButtonUpp:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:ButtonUpp:TabIndex = 0.
      THIS-OBJECT:ButtonUpp:Text = " ".
      THIS-OBJECT:ButtonUpp:Click:Subscribe(THIS-OBJECT:ButtonUpp_Click).
      /*  */
      /* PopBerMtrlPanel */
      /*  */
      THIS-OBJECT:PopBerMtrlPanel:BorderStyle = Infragistics.Win.UIElementBorderStyle:Rounded4.
      /*  */
      /* PopBerMtrlPanel.ClientArea */
      /*  */
      THIS-OBJECT:PopBerMtrlPanel:ClientArea:Controls:Add(THIS-OBJECT:SplitContainerPopBerMtrl).
      THIS-OBJECT:PopBerMtrlPanel:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:PopBerMtrlPanel:Name = "PopBerMtrlPanel".
      THIS-OBJECT:PopBerMtrlPanel:Size = NEW System.Drawing.Size(510, 200).
      THIS-OBJECT:PopBerMtrlPanel:TabIndex = 1.
      THIS-OBJECT:PopBerMtrlPanel:Visible = FALSE.
      /*  */
      /* SplitContainerPopBerMtrl */
      /*  */
      THIS-OBJECT:SplitContainerPopBerMtrl:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:SplitContainerPopBerMtrl:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:SplitContainerPopBerMtrl:Name = "SplitContainerPopBerMtrl".
      THIS-OBJECT:SplitContainerPopBerMtrl:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* SplitContainerPopBerMtrl.Panel1 */
      /*  */
      THIS-OBJECT:SplitContainerPopBerMtrl:Panel1:Controls:Add(THIS-OBJECT:GridBerMtrl).
      /*  */
      /* SplitContainerPopBerMtrl.Panel2 */
      /*  */
      THIS-OBJECT:SplitContainerPopBerMtrl:Panel2:Controls:Add(THIS-OBJECT:PopBerMtrlOK).
      THIS-OBJECT:SplitContainerPopBerMtrl:Size = NEW System.Drawing.Size(506, 196).
      THIS-OBJECT:SplitContainerPopBerMtrl:SplitterDistance = 167.
      THIS-OBJECT:SplitContainerPopBerMtrl:TabIndex = 2.
      /*  */
      /* GridBerMtrl */
      /*  */
      appearance15:BackColor = System.Drawing.SystemColors:Window.
      appearance15:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Appearance = appearance15.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance16:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance16:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance16:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:GroupByBox:Appearance = appearance16.
      appearance17:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:GroupByBox:BandLabelAppearance = appearance17.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance18:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance18:BackColor2 = System.Drawing.SystemColors:Control.
      appearance18:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance18:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:GroupByBox:PromptAppearance = appearance18.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance19:BackColor = System.Drawing.SystemColors:Window.
      appearance19:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Override:ActiveCellAppearance = appearance19.
      appearance20:BackColor = System.Drawing.SystemColors:Highlight.
      appearance20:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Override:ActiveRowAppearance = appearance20.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance21:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Override:CardAreaAppearance = appearance21.
      appearance22:BorderColor = System.Drawing.Color:Silver.
      appearance22:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Override:CellAppearance = appearance22.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Override:CellPadding = 0.
      appearance23:BackColor = System.Drawing.SystemColors:Control.
      appearance23:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance23:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance23:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance23:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Override:GroupByRowAppearance = appearance23.
      appearance24:TextHAlignAsString = "Left".
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Override:HeaderAppearance = appearance24.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance25:BackColor = System.Drawing.SystemColors:Window.
      appearance25:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Override:RowAppearance = appearance25.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance26:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridBerMtrl:DisplayLayout:Override:TemplateAddRowAppearance = appearance26.
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
      THIS-OBJECT:GridBerMtrl:Size = NEW System.Drawing.Size(506, 167).
      THIS-OBJECT:GridBerMtrl:TabIndex = 1.
      THIS-OBJECT:GridBerMtrl:Text = "grid1".
      /*  */
      /* PopBerMtrlOK */
      /*  */
      THIS-OBJECT:PopBerMtrlOK:Anchor = System.Windows.Forms.AnchorStyles:None.
      appearance27:Image = CAST(resources:GetObject("appearance27.Image"), System.Object).
      THIS-OBJECT:PopBerMtrlOK:Appearance = appearance27.
      THIS-OBJECT:PopBerMtrlOK:Location = NEW System.Drawing.Point(395, -2).
      THIS-OBJECT:PopBerMtrlOK:Name = "PopBerMtrlOK".
      THIS-OBJECT:PopBerMtrlOK:Size = NEW System.Drawing.Size(110, 28).
      THIS-OBJECT:PopBerMtrlOK:TabIndex = 3.
      THIS-OBJECT:PopBerMtrlOK:Text = " OK".
      THIS-OBJECT:PopBerMtrlOK:Click:Subscribe(THIS-OBJECT:PopBerMtrlOK_Click).
      /*  */
      /* splitContainerKonst */
      /*  */
      THIS-OBJECT:splitContainerKonst:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKonst:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerKonst:IsSplitterFixed = TRUE.
      THIS-OBJECT:splitContainerKonst:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKonst:Name = "splitContainerKonst".
      THIS-OBJECT:splitContainerKonst:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerKonst.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKonst:Panel1:Controls:Add(THIS-OBJECT:splitContainerAddRemo).
      /*  */
      /* splitContainerKonst.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKonst:Panel2:Controls:Add(THIS-OBJECT:splitContainerValjKonst).
      THIS-OBJECT:splitContainerKonst:Size = NEW System.Drawing.Size(1194, 349).
      THIS-OBJECT:splitContainerKonst:SplitterDistance = 25.
      THIS-OBJECT:splitContainerKonst:TabIndex = 0.
      /*  */
      /* splitContainerAddRemo */
      /*  */
      THIS-OBJECT:splitContainerAddRemo:Location = NEW System.Drawing.Point(5, 0).
      THIS-OBJECT:splitContainerAddRemo:Name = "splitContainerAddRemo".
      /*  */
      /* splitContainerAddRemo.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerAddRemo:Panel2:Controls:Add(THIS-OBJECT:splitContainerNyBort).
      THIS-OBJECT:splitContainerAddRemo:Size = NEW System.Drawing.Size(1194, 25).
      THIS-OBJECT:splitContainerAddRemo:SplitterDistance = 398.
      THIS-OBJECT:splitContainerAddRemo:TabIndex = 5.
      /*  */
      /* splitContainerNyBort */
      /*  */
      THIS-OBJECT:splitContainerNyBort:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerNyBort:Name = "splitContainerNyBort".
      /*  */
      /* splitContainerNyBort.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerNyBort:Panel1:Controls:Add(THIS-OBJECT:AddRemoveStrip).
      /*  */
      /* splitContainerNyBort.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerNyBort:Panel2:Controls:Add(THIS-OBJECT:ButtonNy).
      THIS-OBJECT:splitContainerNyBort:Panel2:Controls:Add(THIS-OBJECT:ButtonBort).
      THIS-OBJECT:splitContainerNyBort:Size = NEW System.Drawing.Size(600, 25).
      THIS-OBJECT:splitContainerNyBort:SplitterDistance = 400.
      THIS-OBJECT:splitContainerNyBort:TabIndex = 3.
      /*  */
      /* AddRemoveStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:RemoveButton.
      arrayvar0[2] = THIS-OBJECT:AddButton.
      arrayvar0[3] = THIS-OBJECT:ClearButton.
      THIS-OBJECT:AddRemoveStrip:Items:AddRange(arrayvar0).
      THIS-OBJECT:AddRemoveStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:AddRemoveStrip:Name = "AddRemoveStrip".
      THIS-OBJECT:AddRemoveStrip:Size = NEW System.Drawing.Size(400, 25).
      THIS-OBJECT:AddRemoveStrip:TabIndex = 4.
      THIS-OBJECT:AddRemoveStrip:Text = "toolStrip1".
      /*  */
      /* RemoveButton */
      /*  */
      THIS-OBJECT:RemoveButton:Image = CAST(resources:GetObject("RemoveButton.Image"), System.Drawing.Image).
      THIS-OBJECT:RemoveButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:RemoveButton:Name = "RemoveButton".
      THIS-OBJECT:RemoveButton:Size = NEW System.Drawing.Size(117, 22).
      THIS-OBJECT:RemoveButton:Text = "Ta bort vald post!".
      THIS-OBJECT:RemoveButton:Click:Subscribe(THIS-OBJECT:RemoveButton_Click).
      /*  */
      /* AddButton */
      /*  */
      THIS-OBJECT:AddButton:Image = CAST(resources:GetObject("AddButton.Image"), System.Drawing.Image).
      THIS-OBJECT:AddButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:AddButton:Name = "AddButton".
      THIS-OBJECT:AddButton:Size = NEW System.Drawing.Size(98, 22).
      THIS-OBJECT:AddButton:Text = "Lägg till post!".
      THIS-OBJECT:AddButton:Click:Subscribe(THIS-OBJECT:AddButton_Click).
      /*  */
      /* ClearButton */
      /*  */
      THIS-OBJECT:ClearButton:Image = CAST(resources:GetObject("ClearButton.Image"), System.Drawing.Image).
      THIS-OBJECT:ClearButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ClearButton:Name = "ClearButton".
      THIS-OBJECT:ClearButton:Size = NEW System.Drawing.Size(153, 22).
      THIS-OBJECT:ClearButton:Text = "Avmarkera samtliga val!".
      THIS-OBJECT:ClearButton:Click:Subscribe(THIS-OBJECT:ClearButton_Click).
      /*  */
      /* ButtonNy */
      /*  */
      appearance28:Image = CAST(resources:GetObject("appearance28.Image"), System.Object).
      THIS-OBJECT:ButtonNy:Appearance = appearance28.
      THIS-OBJECT:ButtonNy:GuruTooltip = "Lägg till post".
      THIS-OBJECT:ButtonNy:Location = NEW System.Drawing.Point(42, 0).
      THIS-OBJECT:ButtonNy:Name = "ButtonNy".
      THIS-OBJECT:ButtonNy:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:ButtonNy:TabIndex = 3.
      THIS-OBJECT:ButtonNy:Text = " ".
      THIS-OBJECT:ButtonNy:Click:Subscribe(THIS-OBJECT:ButtonNy_Click).
      /*  */
      /* ButtonBort */
      /*  */
      appearance29:Image = CAST(resources:GetObject("appearance29.Image"), System.Object).
      THIS-OBJECT:ButtonBort:Appearance = appearance29.
      THIS-OBJECT:ButtonBort:GuruTooltip = "Ta bort vald post".
      THIS-OBJECT:ButtonBort:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:ButtonBort:Name = "ButtonBort".
      THIS-OBJECT:ButtonBort:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:ButtonBort:TabIndex = 2.
      THIS-OBJECT:ButtonBort:Text = " ".
      THIS-OBJECT:ButtonBort:Click:Subscribe(THIS-OBJECT:ButtonBort_Click).
      /*  */
      /* splitContainerValjKonst */
      /*  */
      THIS-OBJECT:splitContainerValjKonst:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerValjKonst:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerValjKonst:Name = "splitContainerValjKonst".
      /*  */
      /* splitContainerValjKonst.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerValjKonst:Panel1:Controls:Add(THIS-OBJECT:splitContaineB1B2B4B5).
      /*  */
      /* splitContainerValjKonst.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerValjKonst:Panel2:Controls:Add(THIS-OBJECT:splitContainerB3B6).
      THIS-OBJECT:splitContainerValjKonst:Size = NEW System.Drawing.Size(1194, 320).
      THIS-OBJECT:splitContainerValjKonst:SplitterDistance = 796.
      THIS-OBJECT:splitContainerValjKonst:TabIndex = 0.
      /*  */
      /* splitContaineB1B2B4B5 */
      /*  */
      THIS-OBJECT:splitContaineB1B2B4B5:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContaineB1B2B4B5:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContaineB1B2B4B5:Name = "splitContaineB1B2B4B5".
      /*  */
      /* splitContaineB1B2B4B5.Panel1 */
      /*  */
      THIS-OBJECT:splitContaineB1B2B4B5:Panel1:Controls:Add(THIS-OBJECT:splitContainerB1B4).
      /*  */
      /* splitContaineB1B2B4B5.Panel2 */
      /*  */
      THIS-OBJECT:splitContaineB1B2B4B5:Panel2:Controls:Add(THIS-OBJECT:splitContainerB2B5).
      THIS-OBJECT:splitContaineB1B2B4B5:Size = NEW System.Drawing.Size(796, 320).
      THIS-OBJECT:splitContaineB1B2B4B5:SplitterDistance = 398.
      THIS-OBJECT:splitContaineB1B2B4B5:TabIndex = 0.
      /*  */
      /* splitContainerB1B4 */
      /*  */
      THIS-OBJECT:splitContainerB1B4:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerB1B4:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerB1B4:Name = "splitContainerB1B4".
      THIS-OBJECT:splitContainerB1B4:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerB1B4.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerB1B4:Panel1:Controls:Add(THIS-OBJECT:GridB1).
      /*  */
      /* splitContainerB1B4.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerB1B4:Panel2:Controls:Add(THIS-OBJECT:GridB4).
      THIS-OBJECT:splitContainerB1B4:Size = NEW System.Drawing.Size(398, 320).
      THIS-OBJECT:splitContainerB1B4:SplitterDistance = 158.
      THIS-OBJECT:splitContainerB1B4:TabIndex = 0.
      /*  */
      /* GridB1 */
      /*  */
      appearance30:BackColor = System.Drawing.SystemColors:Window.
      appearance30:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridB1:DisplayLayout:Appearance = appearance30.
      THIS-OBJECT:GridB1:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridB1:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance31:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance31:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance31:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance31:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridB1:DisplayLayout:GroupByBox:Appearance = appearance31.
      appearance32:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridB1:DisplayLayout:GroupByBox:BandLabelAppearance = appearance32.
      THIS-OBJECT:GridB1:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance33:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance33:BackColor2 = System.Drawing.SystemColors:Control.
      appearance33:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance33:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridB1:DisplayLayout:GroupByBox:PromptAppearance = appearance33.
      THIS-OBJECT:GridB1:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridB1:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridB1:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance34:BackColor = System.Drawing.SystemColors:Window.
      appearance34:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridB1:DisplayLayout:Override:ActiveCellAppearance = appearance34.
      appearance35:BackColor = System.Drawing.SystemColors:Highlight.
      appearance35:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridB1:DisplayLayout:Override:ActiveRowAppearance = appearance35.
      THIS-OBJECT:GridB1:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridB1:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance36:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridB1:DisplayLayout:Override:CardAreaAppearance = appearance36.
      appearance37:BorderColor = System.Drawing.Color:Silver.
      appearance37:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridB1:DisplayLayout:Override:CellAppearance = appearance37.
      THIS-OBJECT:GridB1:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridB1:DisplayLayout:Override:CellPadding = 0.
      appearance38:BackColor = System.Drawing.SystemColors:Control.
      appearance38:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance38:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance38:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance38:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridB1:DisplayLayout:Override:GroupByRowAppearance = appearance38.
      appearance39:TextHAlignAsString = "Left".
      THIS-OBJECT:GridB1:DisplayLayout:Override:HeaderAppearance = appearance39.
      THIS-OBJECT:GridB1:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridB1:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance40:BackColor = System.Drawing.SystemColors:Window.
      appearance40:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridB1:DisplayLayout:Override:RowAppearance = appearance40.
      THIS-OBJECT:GridB1:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance41:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridB1:DisplayLayout:Override:TemplateAddRowAppearance = appearance41.
      THIS-OBJECT:GridB1:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridB1:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridB1:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridB1:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridB1:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridB1:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridB1:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridB1:GuruContext = ?.
      THIS-OBJECT:GridB1:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridB1:Name = "GridB1".
      THIS-OBJECT:GridB1:Size = NEW System.Drawing.Size(398, 158).
      THIS-OBJECT:GridB1:TabIndex = 1.
      THIS-OBJECT:GridB1:Text = "grid2".
      /*  */
      /* GridB4 */
      /*  */
      appearance42:BackColor = System.Drawing.SystemColors:Window.
      appearance42:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridB4:DisplayLayout:Appearance = appearance42.
      THIS-OBJECT:GridB4:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridB4:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance43:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance43:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance43:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance43:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridB4:DisplayLayout:GroupByBox:Appearance = appearance43.
      appearance44:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridB4:DisplayLayout:GroupByBox:BandLabelAppearance = appearance44.
      THIS-OBJECT:GridB4:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance45:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance45:BackColor2 = System.Drawing.SystemColors:Control.
      appearance45:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance45:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridB4:DisplayLayout:GroupByBox:PromptAppearance = appearance45.
      THIS-OBJECT:GridB4:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridB4:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridB4:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance46:BackColor = System.Drawing.SystemColors:Window.
      appearance46:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridB4:DisplayLayout:Override:ActiveCellAppearance = appearance46.
      appearance47:BackColor = System.Drawing.SystemColors:Highlight.
      appearance47:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridB4:DisplayLayout:Override:ActiveRowAppearance = appearance47.
      THIS-OBJECT:GridB4:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridB4:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance48:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridB4:DisplayLayout:Override:CardAreaAppearance = appearance48.
      appearance49:BorderColor = System.Drawing.Color:Silver.
      appearance49:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridB4:DisplayLayout:Override:CellAppearance = appearance49.
      THIS-OBJECT:GridB4:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridB4:DisplayLayout:Override:CellPadding = 0.
      appearance50:BackColor = System.Drawing.SystemColors:Control.
      appearance50:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance50:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance50:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance50:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridB4:DisplayLayout:Override:GroupByRowAppearance = appearance50.
      appearance51:TextHAlignAsString = "Left".
      THIS-OBJECT:GridB4:DisplayLayout:Override:HeaderAppearance = appearance51.
      THIS-OBJECT:GridB4:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridB4:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance52:BackColor = System.Drawing.SystemColors:Window.
      appearance52:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridB4:DisplayLayout:Override:RowAppearance = appearance52.
      THIS-OBJECT:GridB4:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance53:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridB4:DisplayLayout:Override:TemplateAddRowAppearance = appearance53.
      THIS-OBJECT:GridB4:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridB4:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridB4:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridB4:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridB4:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridB4:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridB4:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridB4:GuruContext = ?.
      THIS-OBJECT:GridB4:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridB4:Name = "GridB4".
      THIS-OBJECT:GridB4:Size = NEW System.Drawing.Size(398, 158).
      THIS-OBJECT:GridB4:TabIndex = 2.
      THIS-OBJECT:GridB4:Text = "grid5".
      /*  */
      /* splitContainerB2B5 */
      /*  */
      THIS-OBJECT:splitContainerB2B5:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerB2B5:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerB2B5:Name = "splitContainerB2B5".
      THIS-OBJECT:splitContainerB2B5:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerB2B5.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerB2B5:Panel1:Controls:Add(THIS-OBJECT:GridB2).
      /*  */
      /* splitContainerB2B5.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerB2B5:Panel2:Controls:Add(THIS-OBJECT:GridB5).
      THIS-OBJECT:splitContainerB2B5:Size = NEW System.Drawing.Size(394, 320).
      THIS-OBJECT:splitContainerB2B5:SplitterDistance = 158.
      THIS-OBJECT:splitContainerB2B5:TabIndex = 0.
      /*  */
      /* GridB2 */
      /*  */
      appearance54:BackColor = System.Drawing.SystemColors:Window.
      appearance54:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridB2:DisplayLayout:Appearance = appearance54.
      THIS-OBJECT:GridB2:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridB2:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance55:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance55:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance55:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance55:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridB2:DisplayLayout:GroupByBox:Appearance = appearance55.
      appearance56:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridB2:DisplayLayout:GroupByBox:BandLabelAppearance = appearance56.
      THIS-OBJECT:GridB2:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance57:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance57:BackColor2 = System.Drawing.SystemColors:Control.
      appearance57:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance57:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridB2:DisplayLayout:GroupByBox:PromptAppearance = appearance57.
      THIS-OBJECT:GridB2:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridB2:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridB2:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance58:BackColor = System.Drawing.SystemColors:Window.
      appearance58:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridB2:DisplayLayout:Override:ActiveCellAppearance = appearance58.
      appearance59:BackColor = System.Drawing.SystemColors:Highlight.
      appearance59:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridB2:DisplayLayout:Override:ActiveRowAppearance = appearance59.
      THIS-OBJECT:GridB2:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridB2:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance60:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridB2:DisplayLayout:Override:CardAreaAppearance = appearance60.
      appearance61:BorderColor = System.Drawing.Color:Silver.
      appearance61:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridB2:DisplayLayout:Override:CellAppearance = appearance61.
      THIS-OBJECT:GridB2:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridB2:DisplayLayout:Override:CellPadding = 0.
      appearance62:BackColor = System.Drawing.SystemColors:Control.
      appearance62:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance62:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance62:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance62:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridB2:DisplayLayout:Override:GroupByRowAppearance = appearance62.
      appearance63:TextHAlignAsString = "Left".
      THIS-OBJECT:GridB2:DisplayLayout:Override:HeaderAppearance = appearance63.
      THIS-OBJECT:GridB2:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridB2:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance64:BackColor = System.Drawing.SystemColors:Window.
      appearance64:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridB2:DisplayLayout:Override:RowAppearance = appearance64.
      THIS-OBJECT:GridB2:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance65:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridB2:DisplayLayout:Override:TemplateAddRowAppearance = appearance65.
      THIS-OBJECT:GridB2:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridB2:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridB2:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridB2:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridB2:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridB2:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridB2:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridB2:GuruContext = ?.
      THIS-OBJECT:GridB2:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridB2:Name = "GridB2".
      THIS-OBJECT:GridB2:Size = NEW System.Drawing.Size(394, 158).
      THIS-OBJECT:GridB2:TabIndex = 2.
      THIS-OBJECT:GridB2:Text = "grid3".
      THIS-OBJECT:GridB2:AfterSelectChange:Subscribe(THIS-OBJECT:GridB2_AfterSelectChange).
      /*  */
      /* GridB5 */
      /*  */
      appearance66:BackColor = System.Drawing.SystemColors:Window.
      appearance66:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridB5:DisplayLayout:Appearance = appearance66.
      THIS-OBJECT:GridB5:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridB5:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance67:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance67:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance67:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance67:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridB5:DisplayLayout:GroupByBox:Appearance = appearance67.
      appearance68:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridB5:DisplayLayout:GroupByBox:BandLabelAppearance = appearance68.
      THIS-OBJECT:GridB5:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance69:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance69:BackColor2 = System.Drawing.SystemColors:Control.
      appearance69:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance69:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridB5:DisplayLayout:GroupByBox:PromptAppearance = appearance69.
      THIS-OBJECT:GridB5:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridB5:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridB5:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance70:BackColor = System.Drawing.SystemColors:Window.
      appearance70:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridB5:DisplayLayout:Override:ActiveCellAppearance = appearance70.
      appearance71:BackColor = System.Drawing.SystemColors:Highlight.
      appearance71:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridB5:DisplayLayout:Override:ActiveRowAppearance = appearance71.
      THIS-OBJECT:GridB5:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridB5:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance72:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridB5:DisplayLayout:Override:CardAreaAppearance = appearance72.
      appearance73:BorderColor = System.Drawing.Color:Silver.
      appearance73:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridB5:DisplayLayout:Override:CellAppearance = appearance73.
      THIS-OBJECT:GridB5:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridB5:DisplayLayout:Override:CellPadding = 0.
      appearance74:BackColor = System.Drawing.SystemColors:Control.
      appearance74:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance74:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance74:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance74:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridB5:DisplayLayout:Override:GroupByRowAppearance = appearance74.
      appearance75:TextHAlignAsString = "Left".
      THIS-OBJECT:GridB5:DisplayLayout:Override:HeaderAppearance = appearance75.
      THIS-OBJECT:GridB5:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridB5:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance76:BackColor = System.Drawing.SystemColors:Window.
      appearance76:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridB5:DisplayLayout:Override:RowAppearance = appearance76.
      THIS-OBJECT:GridB5:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance77:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridB5:DisplayLayout:Override:TemplateAddRowAppearance = appearance77.
      THIS-OBJECT:GridB5:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridB5:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridB5:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridB5:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridB5:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridB5:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridB5:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridB5:GuruContext = ?.
      THIS-OBJECT:GridB5:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridB5:Name = "GridB5".
      THIS-OBJECT:GridB5:Size = NEW System.Drawing.Size(394, 158).
      THIS-OBJECT:GridB5:TabIndex = 2.
      THIS-OBJECT:GridB5:Text = "grid6".
      THIS-OBJECT:GridB5:AfterSelectChange:Subscribe(THIS-OBJECT:GridB5_AfterSelectChange).
      /*  */
      /* splitContainerB3B6 */
      /*  */
      THIS-OBJECT:splitContainerB3B6:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerB3B6:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerB3B6:Name = "splitContainerB3B6".
      THIS-OBJECT:splitContainerB3B6:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerB3B6.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerB3B6:Panel1:Controls:Add(THIS-OBJECT:GridB3).
      /*  */
      /* splitContainerB3B6.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerB3B6:Panel2:Controls:Add(THIS-OBJECT:GridB6).
      THIS-OBJECT:splitContainerB3B6:Size = NEW System.Drawing.Size(394, 320).
      THIS-OBJECT:splitContainerB3B6:SplitterDistance = 158.
      THIS-OBJECT:splitContainerB3B6:TabIndex = 0.
      /*  */
      /* GridB3 */
      /*  */
      appearance78:BackColor = System.Drawing.SystemColors:Window.
      appearance78:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridB3:DisplayLayout:Appearance = appearance78.
      THIS-OBJECT:GridB3:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridB3:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance79:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance79:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance79:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance79:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridB3:DisplayLayout:GroupByBox:Appearance = appearance79.
      appearance80:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridB3:DisplayLayout:GroupByBox:BandLabelAppearance = appearance80.
      THIS-OBJECT:GridB3:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance81:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance81:BackColor2 = System.Drawing.SystemColors:Control.
      appearance81:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance81:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridB3:DisplayLayout:GroupByBox:PromptAppearance = appearance81.
      THIS-OBJECT:GridB3:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridB3:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridB3:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance82:BackColor = System.Drawing.SystemColors:Window.
      appearance82:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridB3:DisplayLayout:Override:ActiveCellAppearance = appearance82.
      appearance83:BackColor = System.Drawing.SystemColors:Highlight.
      appearance83:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridB3:DisplayLayout:Override:ActiveRowAppearance = appearance83.
      THIS-OBJECT:GridB3:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridB3:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance84:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridB3:DisplayLayout:Override:CardAreaAppearance = appearance84.
      appearance85:BorderColor = System.Drawing.Color:Silver.
      appearance85:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridB3:DisplayLayout:Override:CellAppearance = appearance85.
      THIS-OBJECT:GridB3:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridB3:DisplayLayout:Override:CellPadding = 0.
      appearance86:BackColor = System.Drawing.SystemColors:Control.
      appearance86:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance86:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance86:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance86:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridB3:DisplayLayout:Override:GroupByRowAppearance = appearance86.
      appearance87:TextHAlignAsString = "Left".
      THIS-OBJECT:GridB3:DisplayLayout:Override:HeaderAppearance = appearance87.
      THIS-OBJECT:GridB3:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridB3:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance88:BackColor = System.Drawing.SystemColors:Window.
      appearance88:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridB3:DisplayLayout:Override:RowAppearance = appearance88.
      THIS-OBJECT:GridB3:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance89:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridB3:DisplayLayout:Override:TemplateAddRowAppearance = appearance89.
      THIS-OBJECT:GridB3:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridB3:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridB3:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridB3:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridB3:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridB3:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridB3:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridB3:GuruContext = ?.
      THIS-OBJECT:GridB3:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridB3:Name = "GridB3".
      THIS-OBJECT:GridB3:Size = NEW System.Drawing.Size(394, 158).
      THIS-OBJECT:GridB3:TabIndex = 2.
      THIS-OBJECT:GridB3:Text = "grid4".
      THIS-OBJECT:GridB3:AfterSelectChange:Subscribe(THIS-OBJECT:GridB3_AfterSelectChange).
      /*  */
      /* GridB6 */
      /*  */
      appearance90:BackColor = System.Drawing.SystemColors:Window.
      appearance90:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridB6:DisplayLayout:Appearance = appearance90.
      THIS-OBJECT:GridB6:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridB6:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance91:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance91:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance91:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance91:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridB6:DisplayLayout:GroupByBox:Appearance = appearance91.
      appearance92:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridB6:DisplayLayout:GroupByBox:BandLabelAppearance = appearance92.
      THIS-OBJECT:GridB6:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance93:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance93:BackColor2 = System.Drawing.SystemColors:Control.
      appearance93:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance93:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridB6:DisplayLayout:GroupByBox:PromptAppearance = appearance93.
      THIS-OBJECT:GridB6:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridB6:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridB6:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance94:BackColor = System.Drawing.SystemColors:Window.
      appearance94:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridB6:DisplayLayout:Override:ActiveCellAppearance = appearance94.
      appearance95:BackColor = System.Drawing.SystemColors:Highlight.
      appearance95:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridB6:DisplayLayout:Override:ActiveRowAppearance = appearance95.
      THIS-OBJECT:GridB6:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridB6:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance96:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridB6:DisplayLayout:Override:CardAreaAppearance = appearance96.
      appearance97:BorderColor = System.Drawing.Color:Silver.
      appearance97:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridB6:DisplayLayout:Override:CellAppearance = appearance97.
      THIS-OBJECT:GridB6:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridB6:DisplayLayout:Override:CellPadding = 0.
      appearance98:BackColor = System.Drawing.SystemColors:Control.
      appearance98:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance98:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance98:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance98:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridB6:DisplayLayout:Override:GroupByRowAppearance = appearance98.
      appearance99:TextHAlignAsString = "Left".
      THIS-OBJECT:GridB6:DisplayLayout:Override:HeaderAppearance = appearance99.
      THIS-OBJECT:GridB6:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridB6:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance100:BackColor = System.Drawing.SystemColors:Window.
      appearance100:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridB6:DisplayLayout:Override:RowAppearance = appearance100.
      THIS-OBJECT:GridB6:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance101:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridB6:DisplayLayout:Override:TemplateAddRowAppearance = appearance101.
      THIS-OBJECT:GridB6:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridB6:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridB6:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridB6:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridB6:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridB6:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridB6:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridB6:GuruContext = ?.
      THIS-OBJECT:GridB6:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridB6:Name = "GridB6".
      THIS-OBJECT:GridB6:Size = NEW System.Drawing.Size(394, 158).
      THIS-OBJECT:GridB6:TabIndex = 2.
      THIS-OBJECT:GridB6:Text = "grid7".
      /*  */
      /* PopMtrlPanel */
      /*  */
      THIS-OBJECT:PopMtrlPanel:BorderStyle = Infragistics.Win.UIElementBorderStyle:Rounded4.
      /*  */
      /* PopMtrlPanel.ClientArea */
      /*  */
      THIS-OBJECT:PopMtrlPanel:ClientArea:Controls:Add(THIS-OBJECT:splitContainerPopMtrlPanel).
      THIS-OBJECT:PopMtrlPanel:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:PopMtrlPanel:Name = "PopMtrlPanel".
      THIS-OBJECT:PopMtrlPanel:Size = NEW System.Drawing.Size(510, 200).
      THIS-OBJECT:PopMtrlPanel:TabIndex = 0.
      THIS-OBJECT:PopMtrlPanel:Visible = FALSE.
      /*  */
      /* splitContainerPopMtrlPanel */
      /*  */
      THIS-OBJECT:splitContainerPopMtrlPanel:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerPopMtrlPanel:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerPopMtrlPanel:Name = "splitContainerPopMtrlPanel".
      THIS-OBJECT:splitContainerPopMtrlPanel:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerPopMtrlPanel.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerPopMtrlPanel:Panel1:Controls:Add(THIS-OBJECT:GridKonMtrl).
      /*  */
      /* splitContainerPopMtrlPanel.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerPopMtrlPanel:Panel2:Controls:Add(THIS-OBJECT:PopMtrlOK).
      THIS-OBJECT:splitContainerPopMtrlPanel:Size = NEW System.Drawing.Size(506, 196).
      THIS-OBJECT:splitContainerPopMtrlPanel:SplitterDistance = 167.
      THIS-OBJECT:splitContainerPopMtrlPanel:TabIndex = 3.
      /*  */
      /* GridKonMtrl */
      /*  */
      appearance102:BackColor = System.Drawing.SystemColors:Window.
      appearance102:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridKonMtrl:DisplayLayout:Appearance = appearance102.
      THIS-OBJECT:GridKonMtrl:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridKonMtrl:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance103:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance103:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance103:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance103:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKonMtrl:DisplayLayout:GroupByBox:Appearance = appearance103.
      appearance104:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKonMtrl:DisplayLayout:GroupByBox:BandLabelAppearance = appearance104.
      THIS-OBJECT:GridKonMtrl:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance105:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance105:BackColor2 = System.Drawing.SystemColors:Control.
      appearance105:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance105:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridKonMtrl:DisplayLayout:GroupByBox:PromptAppearance = appearance105.
      THIS-OBJECT:GridKonMtrl:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridKonMtrl:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridKonMtrl:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance106:BackColor = System.Drawing.SystemColors:Window.
      appearance106:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridKonMtrl:DisplayLayout:Override:ActiveCellAppearance = appearance106.
      appearance107:BackColor = System.Drawing.SystemColors:Highlight.
      appearance107:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridKonMtrl:DisplayLayout:Override:ActiveRowAppearance = appearance107.
      THIS-OBJECT:GridKonMtrl:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridKonMtrl:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance108:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKonMtrl:DisplayLayout:Override:CardAreaAppearance = appearance108.
      appearance109:BorderColor = System.Drawing.Color:Silver.
      appearance109:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridKonMtrl:DisplayLayout:Override:CellAppearance = appearance109.
      THIS-OBJECT:GridKonMtrl:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridKonMtrl:DisplayLayout:Override:CellPadding = 0.
      appearance110:BackColor = System.Drawing.SystemColors:Control.
      appearance110:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance110:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance110:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance110:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridKonMtrl:DisplayLayout:Override:GroupByRowAppearance = appearance110.
      appearance111:TextHAlignAsString = "Left".
      THIS-OBJECT:GridKonMtrl:DisplayLayout:Override:HeaderAppearance = appearance111.
      THIS-OBJECT:GridKonMtrl:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridKonMtrl:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance112:BackColor = System.Drawing.SystemColors:Window.
      appearance112:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridKonMtrl:DisplayLayout:Override:RowAppearance = appearance112.
      THIS-OBJECT:GridKonMtrl:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance113:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridKonMtrl:DisplayLayout:Override:TemplateAddRowAppearance = appearance113.
      THIS-OBJECT:GridKonMtrl:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridKonMtrl:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridKonMtrl:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridKonMtrl:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridKonMtrl:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridKonMtrl:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridKonMtrl:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridKonMtrl:GuruContext = ?.
      THIS-OBJECT:GridKonMtrl:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridKonMtrl:Name = "GridKonMtrl".
      THIS-OBJECT:GridKonMtrl:Size = NEW System.Drawing.Size(506, 167).
      THIS-OBJECT:GridKonMtrl:TabIndex = 0.
      THIS-OBJECT:GridKonMtrl:Text = "grid1".
      /*  */
      /* PopMtrlOK */
      /*  */
      THIS-OBJECT:PopMtrlOK:Anchor = System.Windows.Forms.AnchorStyles:None.
      appearance114:Image = CAST(resources:GetObject("appearance114.Image"), System.Object).
      THIS-OBJECT:PopMtrlOK:Appearance = appearance114.
      THIS-OBJECT:PopMtrlOK:Location = NEW System.Drawing.Point(395, -2).
      THIS-OBJECT:PopMtrlOK:Name = "PopMtrlOK".
      THIS-OBJECT:PopMtrlOK:Size = NEW System.Drawing.Size(110, 28).
      THIS-OBJECT:PopMtrlOK:TabIndex = 3.
      THIS-OBJECT:PopMtrlOK:Text = " OK".
      THIS-OBJECT:PopMtrlOK:Click:Subscribe(THIS-OBJECT:PopMtrlOK_Click).
      /*  */
      /* bindingSource1 */
      /*  */
      THIS-OBJECT:bindingSource1:MaxDataGuess = 0.
      THIS-OBJECT:bindingSource1:NoLOBs = FALSE.
      THIS-OBJECT:bindingSource1:Position = 0.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS "Progress.Data.TableDesc[]" NO-UNDO.
      arrayvar1 = NEW "Progress.Data.TableDesc[]"(0).
      tableDesc1:ChildTables = arrayvar1.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar2 AS Progress.Data.ColumnPropDesc EXTENT 5 NO-UNDO.
      arrayvar2[1] = NEW Progress.Data.ColumnPropDesc("F2", "", Progress.Data.DataType:CHARACTER).
      arrayvar2[2] = NEW Progress.Data.ColumnPropDesc("F3", "", Progress.Data.DataType:CHARACTER).
      arrayvar2[3] = NEW Progress.Data.ColumnPropDesc("F4", "", Progress.Data.DataType:CHARACTER).
      arrayvar2[4] = NEW Progress.Data.ColumnPropDesc("F5", "", Progress.Data.DataType:CHARACTER).
      arrayvar2[5] = NEW Progress.Data.ColumnPropDesc("F6", "", Progress.Data.DataType:CHARACTER).
      tableDesc1:Columns = arrayvar2.
      THIS-OBJECT:bindingSource1:TableSchema = tableDesc1.
      /*  */
      /* ButtonOK */
      /*  */
      THIS-OBJECT:ButtonOK:Anchor = System.Windows.Forms.AnchorStyles:None.
      appearance115:Image = CAST(resources:GetObject("appearance115.Image"), System.Object).
      THIS-OBJECT:ButtonOK:Appearance = appearance115.
      THIS-OBJECT:ButtonOK:Location = NEW System.Drawing.Point(20, 0).
      THIS-OBJECT:ButtonOK:Name = "ButtonOK".
      THIS-OBJECT:ButtonOK:Size = NEW System.Drawing.Size(110, 28).
      THIS-OBJECT:ButtonOK:TabIndex = 2.
      THIS-OBJECT:ButtonOK:TabStop = FALSE.
      THIS-OBJECT:ButtonOK:Text = " OK".
      THIS-OBJECT:ButtonOK:Click:Subscribe(THIS-OBJECT:ButtonOK_Click).
      /*  */
      /* PopupMtrl */
      /*  */
      THIS-OBJECT:PopupMtrl:PopupControl = THIS-OBJECT:PopMtrlPanel.
      /*  */
      /* PopupBerMtrl */
      /*  */
      THIS-OBJECT:PopupBerMtrl:PopupControl = THIS-OBJECT:PopBerMtrlPanel.
      /*  */
      /* splitContainerStart */
      /*  */
      THIS-OBJECT:splitContainerStart:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerStart:FixedPanel = System.Windows.Forms.FixedPanel:Panel2.
      THIS-OBJECT:splitContainerStart:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerStart:Margin = NEW System.Windows.Forms.Padding(0).
      THIS-OBJECT:splitContainerStart:Name = "splitContainerStart".
      THIS-OBJECT:splitContainerStart:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerStart.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerStart:Panel1:Controls:Add(THIS-OBJECT:splitContainerKabelAnd).
      /*  */
      /* splitContainerStart.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerStart:Panel2:Controls:Add(THIS-OBJECT:splitContainerButtunOk).
      THIS-OBJECT:splitContainerStart:Panel2MinSize = 28.
      THIS-OBJECT:splitContainerStart:Size = NEW System.Drawing.Size(1194, 635).
      THIS-OBJECT:splitContainerStart:SplitterDistance = 603.
      THIS-OBJECT:splitContainerStart:TabIndex = 3.
      /*  */
      /* splitContainerButtunOk */
      /*  */
      THIS-OBJECT:splitContainerButtunOk:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerButtunOk:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerButtunOk:Name = "splitContainerButtunOk".
      /*  */
      /* splitContainerButtunOk.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerButtunOk:Panel1:Controls:Add(THIS-OBJECT:BTN_KALK).
      THIS-OBJECT:splitContainerButtunOk:Panel1:Controls:Add(THIS-OBJECT:BTN_SKAP).
      THIS-OBJECT:splitContainerButtunOk:Panel1:Controls:Add(THIS-OBJECT:BTN_MTRL).
      THIS-OBJECT:splitContainerButtunOk:Panel1:Controls:Add(THIS-OBJECT:BTN_ID).
      /*  */
      /* splitContainerButtunOk.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerButtunOk:Panel2:Controls:Add(THIS-OBJECT:ButtonOK).
      THIS-OBJECT:splitContainerButtunOk:Size = NEW System.Drawing.Size(1194, 28).
      THIS-OBJECT:splitContainerButtunOk:SplitterDistance = 1000.
      THIS-OBJECT:splitContainerButtunOk:TabIndex = 3.
      /*  */
      /* BTN_KALK */
      /*  */
      THIS-OBJECT:BTN_KALK:Location = NEW System.Drawing.Point(245, 3).
      THIS-OBJECT:BTN_KALK:Name = "BTN_KALK".
      THIS-OBJECT:BTN_KALK:Size = NEW System.Drawing.Size(80, 23).
      THIS-OBJECT:BTN_KALK:TabIndex = 3.
      THIS-OBJECT:BTN_KALK:Text = "Kalkylera".
      THIS-OBJECT:BTN_KALK:Click:Subscribe(THIS-OBJECT:BTN_KALK_Click).
      /*  */
      /* BTN_SKAP */
      /*  */
      THIS-OBJECT:BTN_SKAP:Location = NEW System.Drawing.Point(165, 3).
      THIS-OBJECT:BTN_SKAP:Name = "BTN_SKAP".
      THIS-OBJECT:BTN_SKAP:Size = NEW System.Drawing.Size(80, 23).
      THIS-OBJECT:BTN_SKAP:TabIndex = 2.
      THIS-OBJECT:BTN_SKAP:Text = "Skåp/Station".
      THIS-OBJECT:BTN_SKAP:Click:Subscribe(THIS-OBJECT:BTN_SKAP_Click).
      /*  */
      /* BTN_MTRL */
      /*  */
      THIS-OBJECT:BTN_MTRL:Location = NEW System.Drawing.Point(85, 3).
      THIS-OBJECT:BTN_MTRL:Name = "BTN_MTRL".
      THIS-OBJECT:BTN_MTRL:Size = NEW System.Drawing.Size(80, 23).
      THIS-OBJECT:BTN_MTRL:TabIndex = 1.
      THIS-OBJECT:BTN_MTRL:Text = "Materiel".
      THIS-OBJECT:BTN_MTRL:Click:Subscribe(THIS-OBJECT:BTN_MTRL_Click).
      /*  */
      /* BTN_ID */
      /*  */
      THIS-OBJECT:BTN_ID:Location = NEW System.Drawing.Point(5, 3).
      THIS-OBJECT:BTN_ID:Name = "BTN_ID".
      THIS-OBJECT:BTN_ID:Size = NEW System.Drawing.Size(80, 23).
      THIS-OBJECT:BTN_ID:TabIndex = 0.
      THIS-OBJECT:BTN_ID:Text = "Identitet".
      THIS-OBJECT:BTN_ID:Click:Subscribe(THIS-OBJECT:BTN_ID_Click).
      /*  */
      /* KabelAnd */
      /*  */
      THIS-OBJECT:ClientSize = NEW System.Drawing.Size(1194, 635).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerStart).
      THIS-OBJECT:FormBorderStyle = System.Windows.Forms.FormBorderStyle:FixedDialog.
      THIS-OBJECT:MaximizeBox = FALSE.
      THIS-OBJECT:MinimizeBox = FALSE.
      THIS-OBJECT:Name = "KabelAnd".
      THIS-OBJECT:ShowInTaskbar = FALSE.
      THIS-OBJECT:StartPosition = System.Windows.Forms.FormStartPosition:CenterParent.
      THIS-OBJECT:Text = "KabelAnd".
      THIS-OBJECT:FormClosing:Subscribe(THIS-OBJECT:KabelAnd_FormClosing).
      THIS-OBJECT:Load:Subscribe(THIS-OBJECT:KabelAnd_Load).
      THIS-OBJECT:Shown:Subscribe(THIS-OBJECT:KabelAnd_Shown).
      THIS-OBJECT:splitContainerKabelAnd:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKabelAnd:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKabelAnd, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKabelAnd:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerValdaKon:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerValdaKon:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerValdaKon:Panel2:PerformLayout().
      CAST(THIS-OBJECT:splitContainerValdaKon, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerValdaKon:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridKabAnd, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:ultraTextEditorOrdning, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:PopBerMtrlPanel:ClientArea:ResumeLayout(FALSE).
      THIS-OBJECT:PopBerMtrlPanel:ResumeLayout(FALSE).
      THIS-OBJECT:SplitContainerPopBerMtrl:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:SplitContainerPopBerMtrl:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:SplitContainerPopBerMtrl, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:SplitContainerPopBerMtrl:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridBerMtrl, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKonst:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKonst:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerKonst, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerKonst:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerAddRemo:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerAddRemo, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerAddRemo:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNyBort:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNyBort:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerNyBort:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerNyBort, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerNyBort:ResumeLayout(FALSE).
      THIS-OBJECT:AddRemoveStrip:ResumeLayout(FALSE).
      THIS-OBJECT:AddRemoveStrip:PerformLayout().
      THIS-OBJECT:splitContainerValjKonst:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerValjKonst:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerValjKonst, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerValjKonst:ResumeLayout(FALSE).
      THIS-OBJECT:splitContaineB1B2B4B5:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContaineB1B2B4B5:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContaineB1B2B4B5, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContaineB1B2B4B5:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerB1B4:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerB1B4:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerB1B4, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerB1B4:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridB1, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridB4, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerB2B5:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerB2B5:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerB2B5, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerB2B5:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridB2, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridB5, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerB3B6:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerB3B6:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerB3B6, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerB3B6:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridB3, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:GridB6, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:PopMtrlPanel:ClientArea:ResumeLayout(FALSE).
      THIS-OBJECT:PopMtrlPanel:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerPopMtrlPanel:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerPopMtrlPanel:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerPopMtrlPanel, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerPopMtrlPanel:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridKonMtrl, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:bindingSource1, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerStart:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerStart:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerStart, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerStart:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerButtunOk:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerButtunOk:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerButtunOk, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerButtunOk:ResumeLayout(FALSE).
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD. 
	METHOD PUBLIC VOID UppNerKabel(INPUT uppner AS LOGICAL ):
	   DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE franskapnum AS INTEGER NO-UNDO.
      DEFINE VARIABLE tillskapnum AS INTEGER NO-UNDO.
      DEFINE VARIABLE franrid AS ROWID NO-UNDO.
      DEFINE VARIABLE tillrid AS ROWID NO-UNDO.
      THIS-OBJECT:KskKon_valTTh:FIND-FIRST("WHERE TTRECID = " + STRING(THIS-OBJECT:GridKabAnd:ActiveRow:Cells["TTRECID"]:VALUE ), NO-LOCK) NO-ERROR.
      IF THIS-OBJECT:ControlDataDb:InkopKoll() = TRUE THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(Guru.konstanter:globalroot:LanguageManager:GetStringAsMessage(194), "").
         RETURN. 
      END.
      franrid = KskKon_valTTh:ROWID.
      franskapnum = THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE.
      IF franskapnum = 0 OR franskapnum = 99 THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(Guru.konstanter:globalroot:LanguageManager:GetStringAsMessage(195), "").
         RETURN. 
      END.  
      IF franskapnum > 99 THEN DO:
         THIS-OBJECT:SkapRid().
         THIS-OBJECT:KskKon_valTTh:FIND-LAST("WHERE NUM = " + STRING(ControlDataDb:BigNumvar) + " AND SKAPNUM < " + STRING(franskapnum) + " AND SKAPNUM NE " + STRING(99) + " USE-INDEX NUM2") NO-ERROR.
         IF THIS-OBJECT:KskKon_valTTh:AVAILABLE THEN tillskapnum = THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE + 1.
         ELSE tillskapnum = 1.
         THIS-OBJECT:KskKon_valTTh:FIND-BY-ROWID(franrid) NO-ERROR.
         THIS-OBJECT:FranTill(tillskapnum).
         THIS-OBJECT:GridKabAnd:GuruReOpen().
         RETURN. 
      END.
      
      franrid = KskKon_valTTh:ROWID.
      franskapnum = THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE. 
      THIS-OBJECT:SkapRid().
     
      IF uppner = TRUE THEN DO:
         THIS-OBJECT:KskKon_valTTh:FIND-LAST("WHERE NUM = " + STRING(ControlDataDb:BigNumvar) + " AND SKAPNUM < " + STRING(franskapnum) + " USE-INDEX NUM2") NO-ERROR.
      END.
	         
	   ELSE DO:
	      THIS-OBJECT:KskKon_valTTh:FIND-FIRST("WHERE NUM = " + STRING(ControlDataDb:BigNumvar) + " AND SKAPNUM > " + STRING(franskapnum) + " USE-INDEX NUM2") NO-ERROR.
	   END. 
	   
      IF THIS-OBJECT:KskKon_valTTh:AVAILABLE THEN DO:
         IF THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE = 0 OR THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE = 99 THEN DO:
            rrr = System.Windows.Forms.MessageBox:Show(Guru.konstanter:globalroot:LanguageManager:GetStringAsMessage(195), "").
            RETURN. 
         END.     
      END.
      ELSE DO:
         rrr = System.Windows.Forms.MessageBox:Show(Guru.konstanter:globalroot:LanguageManager:GetStringAsMessage(195), "").
         RETURN.
      END.  
      tillrid = KskKon_valTTh:ROWID.        
      tillskapnum = THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE.
      THIS-OBJECT:SkapRid().
      THIS-OBJECT:KskKon_valTTh:FIND-BY-ROWID(franrid) NO-ERROR.
      THIS-OBJECT:FranTill(tillskapnum).
      THIS-OBJECT:KskKon_valTTh:FIND-BY-ROWID(tillrid) NO-ERROR.
      THIS-OBJECT:FranTill(franskapnum). 
      THIS-OBJECT:KskKon_valTTh:FIND-BY-ROWID(franrid) NO-ERROR.
        
      THIS-OBJECT:GridKabAnd:GuruReOpen().
      
   END METHOD.
   METHOD PUBLIC VOID FranTill(INPUT frantill AS INTEGER):
      queryvar =  "FOR EACH " + THIS-OBJECT:List_mtrlTTh:TABLE + " WHERE NUM = " + STRING(ControlDataDb:BigNumvar) + 
      " AND SKAPRECID = " + STRING(KskKon_valTTh:RECID).
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:List_mtrlTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:List_mtrlTTh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE = frantill. 
         qH:GET-NEXT().
      END.
      queryvar =  "FOR EACH " + THIS-OBJECT:Fri_tempTTh:TABLE + " WHERE NUM = " + STRING(ControlDataDb:BigNumvar) + 
      " AND SKAPRECID = " + STRING(KskKon_valTTh:RECID).
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:Fri_tempTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:Fri_tempTTh:BUFFER-FIELD("KABNR"):BUFFER-VALUE = frantill.
         qH:GET-NEXT().
      END.
      queryvar =  "FOR EACH " + THIS-OBJECT:Markgrupp_tempTTh:TABLE + " WHERE NUM = " + STRING(ControlDataDb:BigNumvar) + 
      " AND SKAPRECID = " + STRING(KskKon_valTTh:RECID).
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:Markgrupp_tempTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:Markgrupp_tempTTh:BUFFER-FIELD("KABNR"):BUFFER-VALUE = frantill.
         qH:GET-NEXT().
      END.
      
      
      
      THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE = frantill.
   END METHOD.
   METHOD PUBLIC VOID SkapRid():
      queryvar =  "FOR EACH " + THIS-OBJECT:List_mtrlTTh:TABLE + " WHERE NUM = " + STRING(ControlDataDb:BigNumvar) + 
      " AND SKAPNUM = " + STRING(THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE).
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:List_mtrlTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:List_mtrlTTh:BUFFER-FIELD("SKAPRECID"):BUFFER-VALUE = KskKon_valTTh:RECID. 
         qH:GET-NEXT().
      END.
      queryvar =  "FOR EACH " + THIS-OBJECT:Fri_tempTTh:TABLE + " WHERE NUM = " + STRING(ControlDataDb:BigNumvar) + 
      " AND KABNR = " + STRING(THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE).
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:Fri_tempTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:Fri_tempTTh:BUFFER-FIELD("SKAPRECID"):BUFFER-VALUE = KskKon_valTTh:RECID. 
         qH:GET-NEXT().
      END.
      queryvar =  "FOR EACH " + THIS-OBJECT:Markgrupp_tempTTh:TABLE + " WHERE NUM = " + STRING(ControlDataDb:BigNumvar) + 
      " AND KABNR = " + STRING(THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE).
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:Markgrupp_tempTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         THIS-OBJECT:Markgrupp_tempTTh:BUFFER-FIELD("SKAPRECID"):BUFFER-VALUE = KskKon_valTTh:RECID. 
         qH:GET-NEXT().
      END.
      
   END METHOD.
	METHOD PUBLIC VOID MtrUpp1(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
	   IF THIS-OBJECT:GridB1:ActiveRow = ? THEN RETURN.
	   IF THIS-OBJECT:GridB1:AntalRader() = 0 THEN RETURN.
	   THIS-OBJECT:MtrUpp(0).
   END METHOD.
   METHOD PUBLIC VOID MtrUpp2(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      IF THIS-OBJECT:GridB2:ActiveRow = ? THEN RETURN. 
      IF THIS-OBJECT:GridB2:AntalRader() = 0 THEN RETURN.
      THIS-OBJECT:MtrUpp(1).
   END METHOD.
   METHOD PUBLIC VOID MtrUpp3(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      IF THIS-OBJECT:GridB3:ActiveRow = ? THEN RETURN. 
      IF THIS-OBJECT:GridB3:AntalRader() = 0 THEN RETURN.
      THIS-OBJECT:MtrUpp(2).
   END METHOD.
   METHOD PUBLIC VOID MtrUpp4(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      IF THIS-OBJECT:GridB4:ActiveRow = ? THEN RETURN.
      IF THIS-OBJECT:GridB4:AntalRader() = 0 THEN RETURN.
      THIS-OBJECT:MtrUpp(3).
   END METHOD.
   METHOD PUBLIC VOID MtrUpp5(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      IF THIS-OBJECT:GridB5:ActiveRow = ? THEN RETURN.
      IF THIS-OBJECT:GridB5:AntalRader() = 0 THEN RETURN.
      THIS-OBJECT:MtrUpp(4).
   END METHOD.
   
   METHOD PUBLIC VOID MtrUpp6(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      IF THIS-OBJECT:GridB6:AntalRader() = 0 THEN RETURN.
      THIS-OBJECT:MtrUpp(5).
   END METHOD.
   METHOD PUBLIC VOID MtrUpp(INPUT vad AS INTEGER):
      THIS-OBJECT:GridKonMtrl:GuruOrderby("BY ENR ").
      IF vad = 0 THEN DO:
         THIS-OBJECT:GridKonMtrl:GuruFiltrera("upp_mtrlTT.KTYPKOD = '" + THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE + 
         "' AND upp_mtrlTT.F1 = ''
            AND upp_mtrlTT.F2 = ''
            AND upp_mtrlTT.F3 = ''
            AND upp_mtrlTT.F4 = ''
            AND upp_mtrlTT.F5 = ''").
         THIS-OBJECT:GridKonMtrl:SetTitle("Materiel enligt upplägget " + THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE).   
      END.
      IF vad = 1 THEN DO:
         THIS-OBJECT:GridKonMtrl:GuruFiltrera("upp_mtrlTT.KTYPKOD = '" + THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE + 
         "' AND upp_mtrlTT.F1 = '" + THIS-OBJECT:andkonstvaltemp2TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE + 
         "' AND upp_mtrlTT.F2 = ''
            AND upp_mtrlTT.F3 = ''
            AND upp_mtrlTT.F4 = ''
            AND upp_mtrlTT.F5 = ''").
         THIS-OBJECT:GridKonMtrl:SetTitle("Materiel enligt upplägget " + THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE + " " + THIS-OBJECT:andkonstvaltemp2TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE).   
      END.
      IF vad = 2 THEN DO:
         THIS-OBJECT:GridKonMtrl:GuruFiltrera("upp_mtrlTT.KTYPKOD = '" + THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE + 
         "' AND upp_mtrlTT.F1 = ''  
            AND upp_mtrlTT.F2 = '" + THIS-OBJECT:andkonstvaltemp3TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE +
         "' AND upp_mtrlTT.F3 = ''
            AND upp_mtrlTT.F4 = ''
            AND upp_mtrlTT.F5 = ''").
         THIS-OBJECT:GridKonMtrl:SetTitle("Materiel enligt upplägget " + THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE + " " + THIS-OBJECT:andkonstvaltemp3TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE).   
      END.
     
      IF vad = 3 THEN DO:
         THIS-OBJECT:GridKonMtrl:GuruFiltrera("upp_mtrlTT.KTYPKOD = '" + THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE + 
         "' AND upp_mtrlTT.F1 = ''  
            AND upp_mtrlTT.F2 = ''
            AND upp_mtrlTT.F3 = '" + THIS-OBJECT:andkonstvaltemp4TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE +
         "' AND upp_mtrlTT.F4 = ''
            AND upp_mtrlTT.F5 = ''").
         
         /*   
         THIS-OBJECT:GridKonMtrl:GuruFiltrera("upp_mtrlTT.KTYPKOD = '" + THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE + 
         "' AND upp_mtrlTT.F1 = ''  
            AND upp_mtrlTT.F2 = ''
            AND upp_mtrlTT.F3 = " + '"' + THIS-OBJECT:andkonstvaltemp4TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE + '"' + " AND upp_mtrlTT.F4 = ''
            AND upp_mtrlTT.F5 = ''").   
         
         THIS-OBJECT:GridKonMtrl:GuruFiltrera("upp_mtrlTT.KTYPKOD = '" + THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE + 
         "' AND upp_mtrlTT.F1 = ''  
            AND upp_mtrlTT.F2 = ''
            AND upp_mtrlTT.F3 = 'AD70''' AND upp_mtrlTT.F4 = ''
            AND upp_mtrlTT.F5 = ''").
            */
         THIS-OBJECT:GridKonMtrl:SetTitle("Materiel enligt upplägget " + THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE + " " + THIS-OBJECT:andkonstvaltemp4TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE).   
      END.
      IF vad = 4 THEN DO:
         THIS-OBJECT:GridKonMtrl:GuruFiltrera("upp_mtrlTT.KTYPKOD = '" + THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE + 
         "' AND upp_mtrlTT.F1 = ''  
            AND upp_mtrlTT.F2 = ''
            AND upp_mtrlTT.F3 = ''
            AND upp_mtrlTT.F4 = '" + THIS-OBJECT:andkonstvaltemp5TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE +
         "' AND upp_mtrlTT.F5 = ''").
         THIS-OBJECT:GridKonMtrl:SetTitle("Materiel enligt upplägget " + THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE + " " + THIS-OBJECT:andkonstvaltemp5TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE).
      END.
      IF vad = 5 THEN DO:
         THIS-OBJECT:GridKonMtrl:GuruFiltrera("upp_mtrlTT.KTYPKOD = '" + THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE + 
         "' AND upp_mtrlTT.F1 = ''  
            AND upp_mtrlTT.F2 = ''
            AND upp_mtrlTT.F3 = ''
            AND upp_mtrlTT.F4 = ''
            AND upp_mtrlTT.F5 = '" + THIS-OBJECT:andkonstvaltemp6TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE + "'").
         THIS-OBJECT:GridKonMtrl:SetTitle("Materiel enligt upplägget " + THIS-OBJECT:KonstTTh:BUFFER-FIELD("KTYPKOD"):BUFFER-VALUE + " " + THIS-OBJECT:andkonstvaltemp6TTh:BUFFER-FIELD("KVALKOD"):BUFFER-VALUE).   
      END.
      THIS-OBJECT:PopupMtrl:Show().
      THIS-OBJECT:GridKonMtrl:GuruReOpen().
   END METHOD.
   METHOD PUBLIC VOID BerMtrUpp(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      THIS-OBJECT:GridBerMtrl:GuruOrderby("BY ENR ").
      THIS-OBJECT:GridBerMtrl:GuruFiltrera("NUM = " + STRING(THIS-OBJECT:ControlDataDb:BigNumvar) + " AND SKAPNUM = " + STRING(THIS-OBJECT:KskKon_valTTh:BUFFER-FIELD("SKAPNUM"):BUFFER-VALUE)). 
         /*
      THIS-OBJECT:GridBerMtrl:SetTitle("Materiel på vald konstruktionsdel!").
      */
      THIS-OBJECT:PopupBerMtrl:Show().
      
      THIS-OBJECT:GridBerMtrl:GuruReOpen().
      
   END METHOD.
   
   
   
   METHOD PUBLIC VOID KskImpMall(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      DEFINE VARIABLE fildir  AS CHARACTER                         NO-UNDO.
      DEFINE VARIABLE OKvald  AS LOGICAL                           NO-UNDO.
      DEFINE VARIABLE res     AS System.Windows.Forms.DialogResult NO-UNDO.
      fildir = SESSION:TEMP-DIRECTORY.
      {SESSIONTEMPDIR.I}
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
      OS-CREATE-DIR VALUE(fildir) NO-ERROR.
      SYSTEM-DIALOG GET-FILE fildir
         TITLE          "Välj den kaelskåpsmall i excel som Ni vill läsa in"
         FILTERS        "All Files (*.xls;*.xlsx)"  "*.xls;*.xlsx"   
         INITIAL-DIR    fildir
         UPDATE OKvald.      
      IF OKvald = FALSE THEN RETURN.  
      THIS-OBJECT:ExcelImportLista(3,fildir).    
      THIS-OBJECT:GridKabAnd:GuruReopen().
      
   END METHOD.
   
   METHOD PUBLIC VOID ExcelImportLista(vad AS INTEGER,filnamn AS CHARACTER):
      Guru.Konstanter:AmericanSet().
      DEFINE VARIABLE klangd AS INTEGER   NO-UNDO.
      DEFINE VARIABLE invar  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE extrakol AS LOGICAL NO-UNDO.
      DEFINE VARIABLE bnum AS CHARACTER NO-UNDO.
      DEFINE VARIABLE flik1 AS INTEGER NO-UNDO.
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE queryvar2 AS CHARACTER NO-UNDO. 
      DEFINE VARIABLE hjacol AS CHARACTER NO-UNDO.
      DEFINE VARIABLE akod AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lkod AS INTEGER NO-UNDO.
      
      /*ccc
      THIS-OBJECT:KalkdbControl:KalkylimportTTh:EMPTY-TEMP-TABLE() NO-ERROR. 
      THIS-OBJECT:KalkVisaControl:VisaExcel:direkticell = TRUE.
      THIS-OBJECT:KalkVisaControl:VisaExcel:UseMsInterop = FALSE.         
      THIS-OBJECT:KalkVisaControl:VisaExcel:iRad = 1.
      THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A".
      THIS-OBJECT:KalkVisaControl:VisaExcel:ColumnRad().
      THIS-OBJECT:KalkVisaControl:VisaExcel:ReadExcelStart(INPUT filnamn).
      extrakol = FALSE.
      DEBUGGER:SET-BREAK().
      DO WHILE THIS-OBJECT:KalkVisaControl:VisaExcel:iRad <= THIS-OBJECT:KalkVisaControl:VisaExcel:iRadslut:
         invar = THIS-OBJECT:KalkVisaControl:VisaExcel:GetData().
         IF invar = ? THEN.
         ELSE IF invar NE "" THEN DO:
            IF vad = 3 THEN DO:
               IF invar = "Matris" THEN extrakol = TRUE.
               IF extrakol = TRUE THEN DO:
                  IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A" THEN DO:
                     THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                     THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE = TRIM(invar) NO-ERROR.
                  END.   
                  IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "B" THEN DO:                                      
                     THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = TRIM(invar) NO-ERROR.                  
                  END.
                  IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "C" THEN DO:
                     THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = INTEGER(invar) NO-ERROR.
                  END.    
                  IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "E" THEN DO:
                     invar = REPLACE(invar,",",".").
                     THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  END.
               END.
               ELSE DO:   
                  IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A" THEN DO:
                     THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().                        
                     /*importera beroende på om du har klickat i konstruktion eller schakt grid    Lena 20200528*/
                     IF THIS-OBJECT:Root:DatabaseManager:Kalkyl:BerKalkNum > 0 THEN DO:
                        THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("BERNUM"):BUFFER-VALUE = THIS-OBJECT:Root:DatabaseManager:Kalkyl:BerKalkNum.
                     END.
                     ELSE IF THIS-OBJECT:Root:DatabaseManager:Kalkyl:BerSchaktSid > 0  THEN DO:
                        THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("SID"):BUFFER-VALUE = THIS-OBJECT:Root:DatabaseManager:Kalkyl:BerSchaktSid.
                     END.   
                     /* MATRIS ANVÄNDS INTE I BEREDNINGSKALKYL BERNUM OCH SID /Lena 20200528
                     THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE = THIS-OBJECT:CurrentMatris.*/                  
                     THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = TRIM(invar) NO-ERROR.                  
                  END.
                  IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "B" THEN DO:
                     THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = INTEGER(invar) NO-ERROR.
                  END.    
                  IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "C" THEN DO:
                     invar = REPLACE(invar,",",".").
                     THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
                  END.
               END.
            END.      
            ELSE DO:          
               IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A" THEN DO:
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-CREATE().
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE = THIS-OBJECT:CurrentMatris.
                  IF vad = 2 THEN DO:
                     klangd = LENGTH(invar).
                   
                     THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = TRIM(SUBSTRING(invar,1,(klangd - 2))).
                     THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = INTEGER(SUBSTRING(invar,(klangd - 1),2)) NO-ERROR.
                  END.
                  ELSE DO:
                     THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE = TRIM(invar) NO-ERROR.
                  END.
               END.
               IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "B" AND vad = 1 THEN DO:
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("LOPNR"):BUFFER-VALUE = INTEGER(invar) NO-ERROR.
               END.    
               IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "C" THEN DO:
                  invar = REPLACE(invar,",",".").
                  THIS-OBJECT:KalkdbControl:KalkylimportTTh:BUFFER-FIELD("ANTAL"):BUFFER-VALUE = DECIMAL(invar) NO-ERROR.
               END.
            END.                     
         END.
         THIS-OBJECT:KalkVisaControl:VisaExcel:ColRight().                        
         IF THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "F" THEN DO:
            THIS-OBJECT:KalkVisaControl:VisaExcel:cColname = "A". 
            THIS-OBJECT:KalkVisaControl:VisaExcel:Rowdown().
         END.                        
      END.
      THIS-OBJECT:KalkVisaControl:VisaExcel:SlutReadExcel().
      Guru.Konstanter:EuropeanAmericanReset().
      THIS-OBJECT:ImportLista().
      THIS-OBJECT:FelMeddCheck().
     
      */
   END METHOD.
   
   
   METHOD PUBLIC VOID PopBerMtrlOK_Click(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      THIS-OBJECT:PopupBerMtrl:Close().   
   END METHOD.
   
  METHOD PUBLIC VOID PopMtrlOK_Click(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      THIS-OBJECT:PopupMtrl:Close().
   END METHOD.

   METHOD PUBLIC VOID AvmarkeraB1(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      THIS-OBJECT:GridB2:GuruAvmarkeraTot().
      THIS-OBJECT:GridB3:GuruAvmarkeraTot().
      THIS-OBJECT:GridB4:GuruAvmarkeraTot().
      THIS-OBJECT:GridB5:GuruAvmarkeraTot().
      THIS-OBJECT:GridB6:GuruAvmarkeraTot().
   END METHOD.
   METHOD PUBLIC VOID AvmarkeraB2(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      THIS-OBJECT:GridB2:GuruAvmarkeraTot().
   END METHOD.
   METHOD PUBLIC VOID AvmarkeraB3(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      THIS-OBJECT:GridB3:GuruAvmarkeraTot().
   END METHOD.
   METHOD PUBLIC VOID AvmarkeraB4(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      THIS-OBJECT:GridB4:GuruAvmarkeraTot().
   END METHOD.
   METHOD PUBLIC VOID AvmarkeraB5(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      THIS-OBJECT:GridB5:GuruAvmarkeraTot().
   END METHOD.
   METHOD PUBLIC VOID AvmarkeraB6(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      THIS-OBJECT:GridB6:GuruAvmarkeraTot().
   END METHOD.
	METHOD PUBLIC VOID ShowModalDialog(  ):
		
		WAIT-FOR THIS-OBJECT:ShowDialog().

	END METHOD.


 
END CLASS.
