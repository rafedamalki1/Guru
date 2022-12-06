 
 /*------------------------------------------------------------------------
    File        : DarwinPlusUserCon
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Mon Nov 13 13:04:11 CET 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.

 

CLASS  Modules.Storning.DarwinPlusUserCon INHERITS UserControl: 
	
   DEFINE PRIVATE VARIABLE AnvForeStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonBortAnv AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE AnvandareStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonBortAnvFore AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonBortAnvForeOver AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonBortOrg AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonForeOver AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonKopplaOrg AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonKopplaForetag AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonNyAnv AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonNyOrgStrip AS System.Windows.Forms.ToolStripButton NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE GridAnv AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridAnvFore AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE GridForetag AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE ForetagStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE OrgStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE GridOrg AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerAnvandare AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerAnvandareForetag AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainer3 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerForetag AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerOrgStrip AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerOrgForetag AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerGrundUsers AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE HmtRubrikerlista             AS Controls.GridRubrikLista                                        NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.  
   DEFINE PUBLIC VARIABLE ControlDataDb                AS Modules.Storning.DarwinPlusUserDb NO-UNDO.
	DEFINE VARIABLE beforekolnamn  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE beforekolvarde AS CHARACTER NO-UNDO.
   DEFINE VARIABLE Anvbeforekolnamn  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE Anvbeforekolvarde AS CHARACTER NO-UNDO.
   DEFINE VARIABLE Anvcancel AS LOGICAL NO-UNDO. 
	CONSTRUCTOR PUBLIC DarwinPlusUserCon (INPUT r AS Guru.Root  ):
		
		
      SUPER().
      THIS-OBJECT:Root = r.
      THIS-OBJECT:HmtRubrikerlista = NEW Controls.GridRubrikLista().
      THIS-OBJECT:ControlDataDb = THIS-OBJECT:Root:DatabaseManager:DarwinPlusUser.
      InitializeComponent().
      THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
      THIS-OBJECT:buttonForeOver:GuruDublett = FALSE.
      THIS-OBJECT:buttonForeOver:GuruKoppla(THIS-OBJECT:GridForetag,THIS-OBJECT:GridAnvFore).
      THIS-OBJECT:buttonBortAnvForeOver:GuruDublett = FALSE.
      THIS-OBJECT:buttonBortAnvForeOver:GuruKoppla(THIS-OBJECT:GridAnvFore,THIS-OBJECT:GridForetag).
      THIS-OBJECT:buttonBortAnvForeOver:GuruRemoveFrom = TRUE.
      
      
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

	END CONSTRUCTOR.


   METHOD PUBLIC VOID CreateGridOrg():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("ORGID","Orgnr",TRUE,35).
      rubrikergrid[2] = NEW Controls.GridRubrik("NAMN","Organisation",FALSE,250).
      
      THIS-OBJECT:GridOrg:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 2, INPUT THIS-OBJECT:ControlDataDb:OrganisationDPbuffh)).
      THIS-OBJECT:GridOrg:TEXT = "Organisation".  
   END METHOD. 
   
   METHOD PUBLIC VOID CreateGridForetag():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("IdentifikationsnrForetag","Fnr",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("Foretag","Företag",TRUE,250).
      
      THIS-OBJECT:GridForetag:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 2, INPUT THIS-OBJECT:ControlDataDb:ForetagDPbuffh)).
      THIS-OBJECT:GridForetag:TEXT = "Företag".  
   END METHOD. 
   
   METHOD PUBLIC VOID CreateGridAnv():
       {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("ANVANDARNAMN","User",FALSE,100).
      rubrikergrid[2] = NEW Controls.GridRubrik("LOSEN","Password",FALSE,100).
      rubrikergrid[3] = NEW Controls.GridRubrik("FNAMN","Förnamn",FALSE,150).
      rubrikergrid[4] = NEW Controls.GridRubrik("ENAMN","Efternamn",FALSE,150).
      rubrikergrid[5] = NEW Controls.GridRubrik("NAMN","Organisation",TRUE,150).
      THIS-OBJECT:GridAnv:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 5, INPUT THIS-OBJECT:ControlDataDb:AnvDpbuffh)).
      THIS-OBJECT:GridAnv:TEXT = "Användare".  
      THIS-OBJECT:GridAnv:tBS:PositionChanged:Subscribe(ValueGridAnvCh).
   END METHOD.
   METHOD PUBLIC VOID CreateGridAnvFore():
       {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("Foretag","Företag",TRUE,150).
       
      THIS-OBJECT:GridAnvFore:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 1, INPUT THIS-OBJECT:ControlDataDb:AnvForDPbuffh)).
       
      THIS-OBJECT:GridAnvFore:TEXT = "Användarens företag".  
      THIS-OBJECT:ValueGridAnvCh(?,?).
   END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridAnv_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE Anvkolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE Anvkolvarde AS CHARACTER NO-UNDO. 
   
      Anvkolnamn = e:Cell:Column:ToString().
      Anvkolvarde = TRIM(e:Cell:VALUE:ToString()).
      
      IF THIS-OBJECT:GridAnv:Guruegenskap:getRubrik(Anvkolnamn):Readonly = TRUE THEN RETURN.
      IF Anvkolnamn = "ANVANDARNAMN" THEN DO:
         IF Anvcancel = TRUE THEN DO:
            Anvcancel = FALSE.
            RETURN.
         END.
          
         IF Anvkolvarde = ""  THEN DO:
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(221), THIS-OBJECT:Root:LanguageManager:GetString(223), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Error).
            Anvkolvarde = "".
            Anvbeforekolvarde = "".
            Anvcancel = TRUE.
            RETURN. 
         END.
         THIS-OBJECT:ControlDataDb:AnvDpbuffextrah:FIND-FIRST("WHERE  ANVANDARNAMN = " + QUOTER(Anvkolvarde),NO-LOCK) NO-ERROR.
         IF THIS-OBJECT:ControlDataDb:AnvDpbuffextrah:AVAILABLE THEN DO:
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(222), THIS-OBJECT:Root:LanguageManager:GetString(223), System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Error).
             Anvkolvarde = Anvbeforekolvarde.
            Anvbeforekolvarde = "".
            Anvcancel = TRUE.
            THIS-OBJECT:GridAnv:ActiveRow:Cells[Anvbeforekolnamn]:VALUE = Anvkolvarde.
            RETURN.
         END.
         
         THIS-OBJECT:ControlDataDb:AnvDpbuffh:BUFFER-FIELD(Anvkolnamn):BUFFER-VALUE = Anvkolvarde.
         THIS-OBJECT:GridAnv:Guruegenskap:getRubrik(Anvkolnamn):Readonly = TRUE.
         Anvcancel = FALSE.
         RETURN.
      END.
      IF Anvkolnamn = "LOSEN" THEN DO:
         IF Anvbeforekolvarde = "" AND Anvkolvarde = "" THEN RETURN.
         IF Anvbeforekolvarde = "" THEN DO:
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(220), THIS-OBJECT:Root:LanguageManager:GetString(219), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
            IF rrr:ToString() NE "Yes" THEN DO:
               Anvkolvarde = "".
               Anvbeforekolvarde = "".
               Anvcancel = TRUE.
               THIS-OBJECT:GridAnv:ActiveRow:Cells[Anvbeforekolnamn]:VALUE = Anvbeforekolvarde.
               RETURN.
            END.    
         END.
         IF Anvcancel = TRUE THEN DO:
            Anvcancel = FALSE.
            RETURN.
         END.   
         IF Anvkolvarde = "" OR LENGTH(Anvkolvarde) < 5  OR Anvkolvarde = "Byt Lösen!" THEN DO:
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(219), THIS-OBJECT:Root:LanguageManager:GetString(225), System.Windows.Forms.MessageBoxButtons:OK, System.Windows.Forms.MessageBoxIcon:Error).
            THIS-OBJECT:GridAnv:GuruCellValjare("LOSEN").
            RETURN.
         END. 
         THIS-OBJECT:ControlDataDb:AnvDpbuffh:BUFFER-FIELD(Anvkolnamn):BUFFER-VALUE = Anvkolvarde.
         IF THIS-OBJECT:ControlDataDb:AnvDpbuffh:BUFFER-FIELD("ANVANDARNAMN"):BUFFER-VALUE = "" OR THIS-OBJECT:ControlDataDb:AnvDpbuffh:BUFFER-FIELD("ANVANDARNAMN"):BUFFER-VALUE = ? OR THIS-OBJECT:ControlDataDb:AnvDpbuffh:BUFFER-FIELD("ANVANDARNAMN"):BUFFER-VALUE = "Byt User!"
         THEN DO:
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(224), THIS-OBJECT:Root:LanguageManager:GetString(223), System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Error).
            RETURN.
         END.     
         
         THIS-OBJECT:ControlDataDb:SparaDarwinUser().  
        
         THIS-OBJECT:ControlDataDb:LosenDarwinUser(INPUT THIS-OBJECT:ControlDataDb:AnvDpbuffh:BUFFER-FIELD("ANVANDARNAMN "):BUFFER-VALUE, INPUT THIS-OBJECT:ControlDataDb:AnvDpbuffh:BUFFER-FIELD("LOSEN"):BUFFER-VALUE).
        
         
      END.   
      Anvcancel = FALSE.
      THIS-OBJECT:ControlDataDb:AnvDpbuffh:BUFFER-FIELD(Anvkolnamn):BUFFER-VALUE = Anvkolvarde.
     
      
      IF THIS-OBJECT:ControlDataDb:AnvDpbuffh:BUFFER-FIELD("ANVANDARNAMN"):BUFFER-VALUE = "" OR THIS-OBJECT:ControlDataDb:AnvDpbuffh:BUFFER-FIELD("ANVANDARNAMN"):BUFFER-VALUE = ? 
      OR THIS-OBJECT:ControlDataDb:AnvDpbuffh:BUFFER-FIELD("ANVANDARNAMN"):BUFFER-VALUE = "Byt User!"
      THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(224), THIS-OBJECT:Root:LanguageManager:GetString(223), System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Error).
         RETURN.
      END. 
      IF THIS-OBJECT:ControlDataDb:AnvDpbuffh:BUFFER-FIELD("LOSEN"):BUFFER-VALUE = "Byt Lösen!"
      THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(226), THIS-OBJECT:Root:LanguageManager:GetString(223), System.Windows.Forms.MessageBoxButtons:ok, System.Windows.Forms.MessageBoxIcon:Error).
         RETURN.
      END.    
      THIS-OBJECT:ControlDataDb:SparaDarwinUser().        
      RETURN.
		

	END METHOD.

	
	
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridAnv_BeforeCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.BeforeCellUpdateEventArgs ):
		DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      Anvbeforekolnamn = e:Cell:Column:ToString().        
      Anvbeforekolvarde = TRIM(e:Cell:VALUE:ToString()).
      
      IF THIS-OBJECT:GridAnv:Guruegenskap:getRubrik(Anvbeforekolnamn):Readonly = TRUE THEN DO:
        
         e:Cancel = TRUE.
         RETURN.
      END.   
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridOrg_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().
      kolvarde = e:Cell:VALUE:ToString().
      IF THIS-OBJECT:GridOrg:Guruegenskap:getRubrik(kolnamn):Readonly = TRUE THEN RETURN.
      
      THIS-OBJECT:ControlDataDb:OrganisationDPbuffh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
      THIS-OBJECT:ControlDataDb:SparaDarwinUser().        
      RETURN.
		

	END METHOD.

   METHOD PUBLIC VOID ValueGridAnvCh(INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
      /*Se filtrera i Grid*/
      
      IF THIS-OBJECT:ControlDataDb:AnvDpbuffh:AVAILABLE THEN DO:
         
         IF THIS-OBJECT:ControlDataDb:AnvDpbuffh:BUFFER-FIELD("ANVANDARNAMN"):BUFFER-VALUE = "Byt User!" THEN THIS-OBJECT:GridAnv:Guruegenskap:getRubrik("ANVANDARNAMN"):Readonly = FALSE.
         ELSE IF THIS-OBJECT:ControlDataDb:AnvDpbuffh:BUFFER-FIELD("ANVANDARNAMN"):BUFFER-VALUE NE "" THEN THIS-OBJECT:GridAnv:Guruegenskap:getRubrik("ANVANDARNAMN"):Readonly = TRUE.
         ELSE  THIS-OBJECT:GridAnv:Guruegenskap:getRubrik("ANVANDARNAMN"):Readonly = FALSE.
         THIS-OBJECT:GridAnvFore:GuruFiltrera("ANVANDARNAMN  = " + QUOTER(THIS-OBJECT:ControlDataDb:AnvDpbuffh:BUFFER-FIELD("ANVANDARNAMN"):BUFFER-VALUE)).
         THIS-OBJECT:GridAnvFore:GuruFirstrow().
         
      END.
      
   END METHOD.
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonBortAnv_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE anvvar AS CHARACTER NO-UNDO.
		DEFINE VARIABLE rid AS ROWID                             NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      IF THIS-OBJECT:GridAnv:Posn() NE -1 THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
         IF rrr:ToString() NE "Yes" THEN RETURN.
         anvvar = THIS-OBJECT:ControlDataDb:AnvDpbuffh:BUFFER-FIELD("ANVANDARNAMN "):BUFFER-VALUE.
         THIS-OBJECT:ControlDataDb:RemoveAnv(INPUT anvvar).
         THIS-OBJECT:GridAnv:GuruBortrader(FALSE).
         THIS-OBJECT:ControlDataDb:BortDarwinUser(INPUT anvvar).
         THIS-OBJECT:ControlDataDb:SparaDarwinUser().
      END.
      ELSE DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(76), THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(75)).
      END.
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonBortAnvFore_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		
		DEFINE VARIABLE rid AS ROWID                             NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      IF THIS-OBJECT:GridAnvFore:Posn() NE -1 THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
         IF rrr:ToString() NE "Yes" THEN RETURN.
         THIS-OBJECT:buttonBortAnvForeOver:GuruOver().
         THIS-OBJECT:ControlDataDb:SparaDarwinUser().
      END.
      ELSE DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(76), THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(75)).
      END.
		
		RETURN.

	END METHOD.


	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonBortOrg_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE rid AS ROWID                             NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      IF THIS-OBJECT:GridOrg:Posn() NE -1 THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(42), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
         IF rrr:ToString() NE "Yes" THEN RETURN.
         THIS-OBJECT:GridOrg:GuruBortrader(FALSE).
         THIS-OBJECT:ControlDataDb:SparaDarwinUser().
      END.
      ELSE DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(76), THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(75)).
      END.
		RETURN.
	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonKopplaForetag_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   IF THIS-OBJECT:ControlDataDb:AnvDpbuffh:AVAILABLE THEN DO:
      	THIS-OBJECT:buttonForeOver:GuruOver().
      	THIS-OBJECT:ControlDataDb:SkapaAnvForetag().
      	THIS-OBJECT:ControlDataDb:SparaDarwinUser().
      	THIS-OBJECT:buttonForeOver:GuruOverefteruppdat().
         IF THIS-OBJECT:GridAnvFore:SELECTED:Rows:Count = 1 THEN THIS-OBJECT:buttonForeOver:GuruTogrid:GuruCellvaljare("antal").
      END.		
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonKopplaOrg_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      IF THIS-OBJECT:ControlDataDb:AnvDpbuffh:AVAILABLE THEN DO:
         THIS-OBJECT:ControlDataDb:AnvDpbuffh:BUFFER-FIELD("ORGID"):BUFFER-VALUE = THIS-OBJECT:ControlDataDb:OrganisationDPbuffh:BUFFER-FIELD("ORGID"):BUFFER-VALUE.
         THIS-OBJECT:ControlDataDb:AnvDpbuffh:BUFFER-FIELD("NAMN"):BUFFER-VALUE = THIS-OBJECT:ControlDataDb:OrganisationDPbuffh:BUFFER-FIELD("NAMN"):BUFFER-VALUE.
         THIS-OBJECT:GridAnv:GuruReopen().
         THIS-OBJECT:ControlDataDb:SparaDarwinUser().
      END.   
    	RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonNyAnv_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		 DEFINE VARIABLE rid AS ROWID NO-UNDO.
		rid = THIS-OBJECT:ControlDataDb:SkapaAnv().
      THIS-OBJECT:GridAnv:GuruReopen().
      THIS-OBJECT:GridAnv:GuruRepositionto(rid).
      THIS-OBJECT:GridAnv:GuruCellValjare("ANVANDARNAMN").  
      
		RETURN.

	END METHOD.


	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonNyOrgStrip_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE rid AS ROWID NO-UNDO.
      rid = THIS-OBJECT:ControlDataDb:SkapaOrg().
      THIS-OBJECT:GridOrg:GuruReopen().
      THIS-OBJECT:GridOrg:GuruRepositionto(rid).
      THIS-OBJECT:ControlDataDb:SparaDarwinUser().
      THIS-OBJECT:GridOrg:GuruCellValjare("NAMN").   
	   
		RETURN.

	END METHOD.

	METHOD PRIVATE VOID InitializeComponent(  ):
		
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Storning.DarwinPlusUserCon").
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
      THIS-OBJECT:splitContainerGrundUsers = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerOrgForetag = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerOrgStrip = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:OrgStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:ButtonNyOrgStrip = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonBortOrg = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonKopplaOrg = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:GridOrg = NEW Controls.Grid().
      THIS-OBJECT:splitContainerForetag = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:buttonForeOver = NEW Controls.ButtonOver().
      THIS-OBJECT:ForetagStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:ButtonKopplaForetag = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:GridForetag = NEW Controls.Grid().
      THIS-OBJECT:splitContainerAnvandareForetag = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerAnvandare = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:AnvandareStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:ButtonNyAnv = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:ButtonBortAnv = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:GridAnv = NEW Controls.Grid().
      THIS-OBJECT:splitContainer3 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:buttonBortAnvForeOver = NEW Controls.ButtonOver().
      THIS-OBJECT:AnvForeStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:ButtonBortAnvFore = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:GridAnvFore = NEW Controls.Grid().
      CAST(THIS-OBJECT:splitContainerGrundUsers, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerGrundUsers:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerGrundUsers:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerGrundUsers:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerOrgForetag, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerOrgForetag:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerOrgForetag:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerOrgForetag:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerOrgStrip, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerOrgStrip:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerOrgStrip:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerOrgStrip:SuspendLayout().
      THIS-OBJECT:OrgStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridOrg, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerForetag, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerForetag:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerForetag:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerForetag:SuspendLayout().
      THIS-OBJECT:ForetagStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridForetag, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerAnvandareForetag, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerAnvandareForetag:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerAnvandareForetag:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerAnvandareForetag:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerAnvandare, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerAnvandare:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerAnvandare:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerAnvandare:SuspendLayout().
      THIS-OBJECT:AnvandareStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridAnv, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainer3, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainer3:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainer3:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainer3:SuspendLayout().
      THIS-OBJECT:AnvForeStrip:SuspendLayout().
      CAST(THIS-OBJECT:GridAnvFore, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainerGrundUsers */
      /*  */
      THIS-OBJECT:splitContainerGrundUsers:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerGrundUsers:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerGrundUsers:Name = "splitContainerGrundUsers".
      /*  */
      /* splitContainerGrundUsers.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerGrundUsers:Panel1:Controls:Add(THIS-OBJECT:splitContainerOrgForetag).
      /*  */
      /* splitContainerGrundUsers.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerGrundUsers:Panel2:Controls:Add(THIS-OBJECT:splitContainerAnvandareForetag).
      THIS-OBJECT:splitContainerGrundUsers:Size = NEW System.Drawing.Size(1184, 562).
      THIS-OBJECT:splitContainerGrundUsers:SplitterDistance = 400.
      THIS-OBJECT:splitContainerGrundUsers:TabIndex = 0.
      /*  */
      /* splitContainerOrgForetag */
      /*  */
      THIS-OBJECT:splitContainerOrgForetag:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerOrgForetag:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerOrgForetag:Name = "splitContainerOrgForetag".
      /*  */
      /* splitContainerOrgForetag.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerOrgForetag:Panel1:Controls:Add(THIS-OBJECT:splitContainerOrgStrip).
      /*  */
      /* splitContainerOrgForetag.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerOrgForetag:Panel2:Controls:Add(THIS-OBJECT:splitContainerForetag).
      THIS-OBJECT:splitContainerOrgForetag:Size = NEW System.Drawing.Size(400, 562).
      THIS-OBJECT:splitContainerOrgForetag:SplitterDistance = 225.
      THIS-OBJECT:splitContainerOrgForetag:TabIndex = 0.
      /*  */
      /* splitContainerOrgStrip */
      /*  */
      THIS-OBJECT:splitContainerOrgStrip:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerOrgStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerOrgStrip:Name = "splitContainerOrgStrip".
      THIS-OBJECT:splitContainerOrgStrip:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerOrgStrip.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerOrgStrip:Panel1:Controls:Add(THIS-OBJECT:OrgStrip).
      THIS-OBJECT:splitContainerOrgStrip:Panel1MinSize = 10.
      /*  */
      /* splitContainerOrgStrip.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerOrgStrip:Panel2:Controls:Add(THIS-OBJECT:GridOrg).
      THIS-OBJECT:splitContainerOrgStrip:Size = NEW System.Drawing.Size(225, 562).
      THIS-OBJECT:splitContainerOrgStrip:SplitterDistance = 27.
      THIS-OBJECT:splitContainerOrgStrip:TabIndex = 0.
      /*  */
      /* OrgStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:ButtonNyOrgStrip.
      arrayvar0[2] = THIS-OBJECT:ButtonBortOrg.
      arrayvar0[3] = THIS-OBJECT:ButtonKopplaOrg.
      THIS-OBJECT:OrgStrip:Items:AddRange(arrayvar0).
      THIS-OBJECT:OrgStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:OrgStrip:Name = "OrgStrip".
      THIS-OBJECT:OrgStrip:Size = NEW System.Drawing.Size(225, 25).
      THIS-OBJECT:OrgStrip:TabIndex = 1.
      THIS-OBJECT:OrgStrip:Text = "toolStrip1".
      /*  */
      /* ButtonNyOrgStrip */
      /*  */
      THIS-OBJECT:ButtonNyOrgStrip:Image = CAST(resources:GetObject("ButtonNyOrgStrip.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonNyOrgStrip:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonNyOrgStrip:Name = "ButtonNyOrgStrip".
      THIS-OBJECT:ButtonNyOrgStrip:Size = NEW System.Drawing.Size(66, 22).
      THIS-OBJECT:ButtonNyOrgStrip:Text = "Ny org.".
      THIS-OBJECT:ButtonNyOrgStrip:Click:Subscribe(THIS-OBJECT:ButtonNyOrgStrip_Click).
      /*  */
      /* ButtonBortOrg */
      /*  */
      THIS-OBJECT:ButtonBortOrg:Image = CAST(resources:GetObject("ButtonBortOrg.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonBortOrg:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonBortOrg:Name = "ButtonBortOrg".
      THIS-OBJECT:ButtonBortOrg:Size = NEW System.Drawing.Size(89, 22).
      THIS-OBJECT:ButtonBortOrg:Text = "Ta bort org.".
      THIS-OBJECT:ButtonBortOrg:Click:Subscribe(THIS-OBJECT:ButtonBortOrg_Click).
      /*  */
      /* ButtonKopplaOrg */
      /*  */
      THIS-OBJECT:ButtonKopplaOrg:Image = CAST(resources:GetObject("ButtonKopplaOrg.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonKopplaOrg:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonKopplaOrg:Name = "ButtonKopplaOrg".
      THIS-OBJECT:ButtonKopplaOrg:Size = NEW System.Drawing.Size(126, 20).
      THIS-OBJECT:ButtonKopplaOrg:Text = "Koppla org. till anv".
      THIS-OBJECT:ButtonKopplaOrg:Click:Subscribe(THIS-OBJECT:ButtonKopplaOrg_Click).
      /*  */
      /* GridOrg */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridOrg:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:GridOrg:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridOrg:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridOrg:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridOrg:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:GridOrg:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridOrg:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:GridOrg:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridOrg:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridOrg:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridOrg:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridOrg:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:GridOrg:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridOrg:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridOrg:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridOrg:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:GridOrg:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridOrg:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridOrg:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:GridOrg:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:GridOrg:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridOrg:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridOrg:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:GridOrg:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridOrg:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:GridOrg:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridOrg:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridOrg:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridOrg:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridOrg:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridOrg:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridOrg:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridOrg:GuruContext = ?.
      THIS-OBJECT:GridOrg:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridOrg:Name = "GridOrg".
      THIS-OBJECT:GridOrg:Size = NEW System.Drawing.Size(225, 531).
      THIS-OBJECT:GridOrg:TabIndex = 1.
      THIS-OBJECT:GridOrg:Text = "grid1".
      THIS-OBJECT:GridOrg:AfterCellUpdate:Subscribe(THIS-OBJECT:GridOrg_AfterCellUpdate).
      /*  */
      /* splitContainerForetag */
      /*  */
      THIS-OBJECT:splitContainerForetag:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerForetag:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerForetag:Name = "splitContainerForetag".
      THIS-OBJECT:splitContainerForetag:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerForetag.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerForetag:Panel1:Controls:Add(THIS-OBJECT:buttonForeOver).
      THIS-OBJECT:splitContainerForetag:Panel1:Controls:Add(THIS-OBJECT:ForetagStrip).
      THIS-OBJECT:splitContainerForetag:Panel1MinSize = 10.
      /*  */
      /* splitContainerForetag.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerForetag:Panel2:Controls:Add(THIS-OBJECT:GridForetag).
      THIS-OBJECT:splitContainerForetag:Size = NEW System.Drawing.Size(171, 562).
      THIS-OBJECT:splitContainerForetag:SplitterDistance = 27.
      THIS-OBJECT:splitContainerForetag:TabIndex = 1.
      /*  */
      /* buttonForeOver */
      /*  */
      THIS-OBJECT:buttonForeOver:Location = NEW System.Drawing.Point(76, 4).
      THIS-OBJECT:buttonForeOver:Name = "buttonForeOver".
      THIS-OBJECT:buttonForeOver:Size = NEW System.Drawing.Size(75, 23).
      THIS-OBJECT:buttonForeOver:TabIndex = 1.
      THIS-OBJECT:buttonForeOver:Text = "Lägg till Företag".
      THIS-OBJECT:buttonForeOver:Visible = FALSE.
      /*  */
      /* ForetagStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS System.Windows.Forms.ToolStripItem EXTENT 1 NO-UNDO.
      arrayvar1[1] = THIS-OBJECT:ButtonKopplaForetag.
      THIS-OBJECT:ForetagStrip:Items:AddRange(arrayvar1).
      THIS-OBJECT:ForetagStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:ForetagStrip:Name = "ForetagStrip".
      THIS-OBJECT:ForetagStrip:Size = NEW System.Drawing.Size(171, 25).
      THIS-OBJECT:ForetagStrip:TabIndex = 0.
      THIS-OBJECT:ForetagStrip:Text = "toolStrip1".
      /*  */
      /* ButtonKopplaForetag */
      /*  */
      THIS-OBJECT:ButtonKopplaForetag:Image = CAST(resources:GetObject("ButtonKopplaForetag.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonKopplaForetag:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonKopplaForetag:Name = "ButtonKopplaForetag".
      THIS-OBJECT:ButtonKopplaForetag:Size = NEW System.Drawing.Size(179, 20).
      THIS-OBJECT:ButtonKopplaForetag:Text = "Koppla företag till användare".
      THIS-OBJECT:ButtonKopplaForetag:Click:Subscribe(THIS-OBJECT:ButtonKopplaForetag_Click).
      /*  */
      /* GridForetag */
      /*  */
      appearance13:BackColor = System.Drawing.SystemColors:Window.
      appearance13:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridForetag:DisplayLayout:Appearance = appearance13.
      THIS-OBJECT:GridForetag:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridForetag:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance14:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance14:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance14:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance14:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridForetag:DisplayLayout:GroupByBox:Appearance = appearance14.
      appearance15:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridForetag:DisplayLayout:GroupByBox:BandLabelAppearance = appearance15.
      THIS-OBJECT:GridForetag:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance16:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance16:BackColor2 = System.Drawing.SystemColors:Control.
      appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance16:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridForetag:DisplayLayout:GroupByBox:PromptAppearance = appearance16.
      THIS-OBJECT:GridForetag:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridForetag:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridForetag:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance17:BackColor = System.Drawing.SystemColors:Window.
      appearance17:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridForetag:DisplayLayout:Override:ActiveCellAppearance = appearance17.
      appearance18:BackColor = System.Drawing.SystemColors:Highlight.
      appearance18:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridForetag:DisplayLayout:Override:ActiveRowAppearance = appearance18.
      THIS-OBJECT:GridForetag:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridForetag:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance19:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridForetag:DisplayLayout:Override:CardAreaAppearance = appearance19.
      appearance20:BorderColor = System.Drawing.Color:Silver.
      appearance20:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridForetag:DisplayLayout:Override:CellAppearance = appearance20.
      THIS-OBJECT:GridForetag:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridForetag:DisplayLayout:Override:CellPadding = 0.
      appearance21:BackColor = System.Drawing.SystemColors:Control.
      appearance21:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance21:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance21:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance21:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridForetag:DisplayLayout:Override:GroupByRowAppearance = appearance21.
      appearance22:TextHAlignAsString = "Left".
      THIS-OBJECT:GridForetag:DisplayLayout:Override:HeaderAppearance = appearance22.
      THIS-OBJECT:GridForetag:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridForetag:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance23:BackColor = System.Drawing.SystemColors:Window.
      appearance23:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridForetag:DisplayLayout:Override:RowAppearance = appearance23.
      THIS-OBJECT:GridForetag:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance24:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridForetag:DisplayLayout:Override:TemplateAddRowAppearance = appearance24.
      THIS-OBJECT:GridForetag:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridForetag:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridForetag:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridForetag:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridForetag:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridForetag:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridForetag:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridForetag:GuruContext = ?.
      THIS-OBJECT:GridForetag:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridForetag:Name = "GridForetag".
      THIS-OBJECT:GridForetag:Size = NEW System.Drawing.Size(171, 531).
      THIS-OBJECT:GridForetag:TabIndex = 0.
      THIS-OBJECT:GridForetag:Text = "grid1".
      /*  */
      /* splitContainerAnvandareForetag */
      /*  */
      THIS-OBJECT:splitContainerAnvandareForetag:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerAnvandareForetag:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerAnvandareForetag:Name = "splitContainerAnvandareForetag".
      /*  */
      /* splitContainerAnvandareForetag.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerAnvandareForetag:Panel1:Controls:Add(THIS-OBJECT:splitContainerAnvandare).
      /*  */
      /* splitContainerAnvandareForetag.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerAnvandareForetag:Panel2:Controls:Add(THIS-OBJECT:splitContainer3).
      THIS-OBJECT:splitContainerAnvandareForetag:Size = NEW System.Drawing.Size(780, 562).
      THIS-OBJECT:splitContainerAnvandareForetag:SplitterDistance = 600.
      THIS-OBJECT:splitContainerAnvandareForetag:TabIndex = 1.
      /*  */
      /* splitContainerAnvandare */
      /*  */
      THIS-OBJECT:splitContainerAnvandare:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerAnvandare:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerAnvandare:Name = "splitContainerAnvandare".
      THIS-OBJECT:splitContainerAnvandare:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerAnvandare.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerAnvandare:Panel1:Controls:Add(THIS-OBJECT:AnvandareStrip).
      THIS-OBJECT:splitContainerAnvandare:Panel1MinSize = 10.
      /*  */
      /* splitContainerAnvandare.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerAnvandare:Panel2:Controls:Add(THIS-OBJECT:GridAnv).
      THIS-OBJECT:splitContainerAnvandare:Size = NEW System.Drawing.Size(600, 562).
      THIS-OBJECT:splitContainerAnvandare:SplitterDistance = 27.
      THIS-OBJECT:splitContainerAnvandare:TabIndex = 0.
      /*  */
      /* AnvandareStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar2 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar2[1] = THIS-OBJECT:ButtonNyAnv.
      arrayvar2[2] = THIS-OBJECT:ButtonBortAnv.
      THIS-OBJECT:AnvandareStrip:Items:AddRange(arrayvar2).
      THIS-OBJECT:AnvandareStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:AnvandareStrip:Name = "AnvandareStrip".
      THIS-OBJECT:AnvandareStrip:Size = NEW System.Drawing.Size(600, 25).
      THIS-OBJECT:AnvandareStrip:TabIndex = 0.
      THIS-OBJECT:AnvandareStrip:Text = "toolStrip1".
      /*  */
      /* ButtonNyAnv */
      /*  */
      THIS-OBJECT:ButtonNyAnv:Image = CAST(resources:GetObject("ButtonNyAnv.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonNyAnv:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonNyAnv:Name = "ButtonNyAnv".
      THIS-OBJECT:ButtonNyAnv:Size = NEW System.Drawing.Size(143, 22).
      THIS-OBJECT:ButtonNyAnv:Text = "Lägg till en användare".
      THIS-OBJECT:ButtonNyAnv:Click:Subscribe(THIS-OBJECT:ButtonNyAnv_Click).
      /*  */
      /* ButtonBortAnv */
      /*  */
      THIS-OBJECT:ButtonBortAnv:Image = CAST(resources:GetObject("ButtonBortAnv.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonBortAnv:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonBortAnv:Name = "ButtonBortAnv".
      THIS-OBJECT:ButtonBortAnv:Size = NEW System.Drawing.Size(123, 22).
      THIS-OBJECT:ButtonBortAnv:Text = "Ta bort användare".
      THIS-OBJECT:ButtonBortAnv:Click:Subscribe(THIS-OBJECT:ButtonBortAnv_Click).
      /*  */
      /* GridAnv */
      /*  */
      appearance25:BackColor = System.Drawing.SystemColors:Window.
      appearance25:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridAnv:DisplayLayout:Appearance = appearance25.
      THIS-OBJECT:GridAnv:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridAnv:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance26:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance26:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance26:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance26:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridAnv:DisplayLayout:GroupByBox:Appearance = appearance26.
      appearance27:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridAnv:DisplayLayout:GroupByBox:BandLabelAppearance = appearance27.
      THIS-OBJECT:GridAnv:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance28:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance28:BackColor2 = System.Drawing.SystemColors:Control.
      appearance28:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance28:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridAnv:DisplayLayout:GroupByBox:PromptAppearance = appearance28.
      THIS-OBJECT:GridAnv:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridAnv:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridAnv:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance29:BackColor = System.Drawing.SystemColors:Window.
      appearance29:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridAnv:DisplayLayout:Override:ActiveCellAppearance = appearance29.
      appearance30:BackColor = System.Drawing.SystemColors:Highlight.
      appearance30:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridAnv:DisplayLayout:Override:ActiveRowAppearance = appearance30.
      THIS-OBJECT:GridAnv:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridAnv:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance31:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridAnv:DisplayLayout:Override:CardAreaAppearance = appearance31.
      appearance32:BorderColor = System.Drawing.Color:Silver.
      appearance32:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridAnv:DisplayLayout:Override:CellAppearance = appearance32.
      THIS-OBJECT:GridAnv:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridAnv:DisplayLayout:Override:CellPadding = 0.
      appearance33:BackColor = System.Drawing.SystemColors:Control.
      appearance33:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance33:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance33:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance33:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridAnv:DisplayLayout:Override:GroupByRowAppearance = appearance33.
      appearance34:TextHAlignAsString = "Left".
      THIS-OBJECT:GridAnv:DisplayLayout:Override:HeaderAppearance = appearance34.
      THIS-OBJECT:GridAnv:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridAnv:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance35:BackColor = System.Drawing.SystemColors:Window.
      appearance35:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridAnv:DisplayLayout:Override:RowAppearance = appearance35.
      THIS-OBJECT:GridAnv:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance36:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridAnv:DisplayLayout:Override:TemplateAddRowAppearance = appearance36.
      THIS-OBJECT:GridAnv:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridAnv:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridAnv:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridAnv:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridAnv:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridAnv:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridAnv:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridAnv:GuruContext = ?.
      THIS-OBJECT:GridAnv:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridAnv:Name = "GridAnv".
      THIS-OBJECT:GridAnv:Size = NEW System.Drawing.Size(600, 531).
      THIS-OBJECT:GridAnv:TabIndex = 0.
      THIS-OBJECT:GridAnv:Text = "grid1".
      THIS-OBJECT:GridAnv:AfterCellUpdate:Subscribe(THIS-OBJECT:GridAnv_AfterCellUpdate).
      THIS-OBJECT:GridAnv:BeforeCellUpdate:Subscribe(THIS-OBJECT:GridAnv_BeforeCellUpdate).
      /*  */
      /* splitContainer3 */
      /*  */
      THIS-OBJECT:splitContainer3:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainer3:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainer3:Name = "splitContainer3".
      THIS-OBJECT:splitContainer3:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainer3.Panel1 */
      /*  */
      THIS-OBJECT:splitContainer3:Panel1:Controls:Add(THIS-OBJECT:buttonBortAnvForeOver).
      THIS-OBJECT:splitContainer3:Panel1:Controls:Add(THIS-OBJECT:AnvForeStrip).
      THIS-OBJECT:splitContainer3:Panel1MinSize = 10.
      /*  */
      /* splitContainer3.Panel2 */
      /*  */
      THIS-OBJECT:splitContainer3:Panel2:Controls:Add(THIS-OBJECT:GridAnvFore).
      THIS-OBJECT:splitContainer3:Size = NEW System.Drawing.Size(176, 562).
      THIS-OBJECT:splitContainer3:SplitterDistance = 27.
      THIS-OBJECT:splitContainer3:TabIndex = 1.
      /*  */
      /* buttonBortAnvForeOver */
      /*  */
      THIS-OBJECT:buttonBortAnvForeOver:Location = NEW System.Drawing.Point(74, 1).
      THIS-OBJECT:buttonBortAnvForeOver:Name = "buttonBortAnvForeOver".
      THIS-OBJECT:buttonBortAnvForeOver:Size = NEW System.Drawing.Size(75, 23).
      THIS-OBJECT:buttonBortAnvForeOver:TabIndex = 1.
      THIS-OBJECT:buttonBortAnvForeOver:Text = "TaBortAnvFore".
      THIS-OBJECT:buttonBortAnvForeOver:Visible = FALSE.
      /*  */
      /* AnvForeStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar3 AS System.Windows.Forms.ToolStripItem EXTENT 1 NO-UNDO.
      arrayvar3[1] = THIS-OBJECT:ButtonBortAnvFore.
      THIS-OBJECT:AnvForeStrip:Items:AddRange(arrayvar3).
      THIS-OBJECT:AnvForeStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:AnvForeStrip:Name = "AnvForeStrip".
      THIS-OBJECT:AnvForeStrip:Size = NEW System.Drawing.Size(176, 25).
      THIS-OBJECT:AnvForeStrip:TabIndex = 0.
      THIS-OBJECT:AnvForeStrip:Text = "toolStrip1".
      /*  */
      /* ButtonBortAnvFore */
      /*  */
      THIS-OBJECT:ButtonBortAnvFore:Image = CAST(resources:GetObject("ButtonBortAnvFore.Image"), System.Drawing.Image).
      THIS-OBJECT:ButtonBortAnvFore:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:ButtonBortAnvFore:Name = "ButtonBortAnvFore".
      THIS-OBJECT:ButtonBortAnvFore:Size = NEW System.Drawing.Size(195, 20).
      THIS-OBJECT:ButtonBortAnvFore:Text = "Ta bort kopplingen till företaget".
      THIS-OBJECT:ButtonBortAnvFore:Click:Subscribe(THIS-OBJECT:ButtonBortAnvFore_Click).
      /*  */
      /* GridAnvFore */
      /*  */
      appearance37:BackColor = System.Drawing.SystemColors:Window.
      appearance37:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridAnvFore:DisplayLayout:Appearance = appearance37.
      THIS-OBJECT:GridAnvFore:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridAnvFore:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance38:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance38:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance38:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance38:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridAnvFore:DisplayLayout:GroupByBox:Appearance = appearance38.
      appearance39:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridAnvFore:DisplayLayout:GroupByBox:BandLabelAppearance = appearance39.
      THIS-OBJECT:GridAnvFore:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance40:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance40:BackColor2 = System.Drawing.SystemColors:Control.
      appearance40:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance40:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridAnvFore:DisplayLayout:GroupByBox:PromptAppearance = appearance40.
      THIS-OBJECT:GridAnvFore:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridAnvFore:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridAnvFore:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance41:BackColor = System.Drawing.SystemColors:Window.
      appearance41:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridAnvFore:DisplayLayout:Override:ActiveCellAppearance = appearance41.
      appearance42:BackColor = System.Drawing.SystemColors:Highlight.
      appearance42:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridAnvFore:DisplayLayout:Override:ActiveRowAppearance = appearance42.
      THIS-OBJECT:GridAnvFore:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridAnvFore:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance43:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridAnvFore:DisplayLayout:Override:CardAreaAppearance = appearance43.
      appearance44:BorderColor = System.Drawing.Color:Silver.
      appearance44:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridAnvFore:DisplayLayout:Override:CellAppearance = appearance44.
      THIS-OBJECT:GridAnvFore:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridAnvFore:DisplayLayout:Override:CellPadding = 0.
      appearance45:BackColor = System.Drawing.SystemColors:Control.
      appearance45:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance45:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance45:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance45:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridAnvFore:DisplayLayout:Override:GroupByRowAppearance = appearance45.
      appearance46:TextHAlignAsString = "Left".
      THIS-OBJECT:GridAnvFore:DisplayLayout:Override:HeaderAppearance = appearance46.
      THIS-OBJECT:GridAnvFore:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridAnvFore:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance47:BackColor = System.Drawing.SystemColors:Window.
      appearance47:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridAnvFore:DisplayLayout:Override:RowAppearance = appearance47.
      THIS-OBJECT:GridAnvFore:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance48:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridAnvFore:DisplayLayout:Override:TemplateAddRowAppearance = appearance48.
      THIS-OBJECT:GridAnvFore:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridAnvFore:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridAnvFore:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridAnvFore:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridAnvFore:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridAnvFore:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridAnvFore:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridAnvFore:GuruContext = ?.
      THIS-OBJECT:GridAnvFore:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridAnvFore:Name = "GridAnvFore".
      THIS-OBJECT:GridAnvFore:Size = NEW System.Drawing.Size(176, 531).
      THIS-OBJECT:GridAnvFore:TabIndex = 0.
      THIS-OBJECT:GridAnvFore:Text = "grid1".
      /*  */
      /* DarwinPlusUserCon */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerGrundUsers).
      THIS-OBJECT:Name = "DarwinPlusUserCon".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1184, 562).
      THIS-OBJECT:Click:Subscribe(THIS-OBJECT:ButtonNyOrgStrip_Click).
      THIS-OBJECT:splitContainerGrundUsers:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerGrundUsers:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerGrundUsers, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerGrundUsers:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerOrgForetag:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerOrgForetag:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerOrgForetag, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerOrgForetag:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerOrgStrip:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerOrgStrip:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerOrgStrip:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerOrgStrip, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerOrgStrip:ResumeLayout(FALSE).
      THIS-OBJECT:OrgStrip:ResumeLayout(FALSE).
      THIS-OBJECT:OrgStrip:PerformLayout().
      CAST(THIS-OBJECT:GridOrg, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerForetag:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerForetag:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerForetag:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerForetag, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerForetag:ResumeLayout(FALSE).
      THIS-OBJECT:ForetagStrip:ResumeLayout(FALSE).
      THIS-OBJECT:ForetagStrip:PerformLayout().
      CAST(THIS-OBJECT:GridForetag, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerAnvandareForetag:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerAnvandareForetag:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerAnvandareForetag, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerAnvandareForetag:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerAnvandare:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerAnvandare:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerAnvandare:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerAnvandare, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerAnvandare:ResumeLayout(FALSE).
      THIS-OBJECT:AnvandareStrip:ResumeLayout(FALSE).
      THIS-OBJECT:AnvandareStrip:PerformLayout().
      CAST(THIS-OBJECT:GridAnv, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainer3:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainer3:Panel1:PerformLayout().
      THIS-OBJECT:splitContainer3:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainer3, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainer3:ResumeLayout(FALSE).
      THIS-OBJECT:AnvForeStrip:ResumeLayout(FALSE).
      THIS-OBJECT:AnvForeStrip:PerformLayout().
      CAST(THIS-OBJECT:GridAnvFore, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.

	DESTRUCTOR PUBLIC DarwinPlusUserCon ( ):

	END DESTRUCTOR.

END CLASS.