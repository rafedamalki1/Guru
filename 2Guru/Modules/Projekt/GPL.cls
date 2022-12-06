 
 /*------------------------------------------------------------------------
    File        : GPL
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Fri Apr 22 09:34:45 CEST 2016
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.
USING Progress.Windows.Form.



CLASS Modules.Projekt.GPL INHERITS UserControl: 
   
   DEFINE PRIVATE VARIABLE Borgnr AS Controls.FillinChar NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonCancelErsatt AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE buttonCancelMakulera AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonOK AS Infragistics.Win.Misc.UltraButton NO-UNDO.
   DEFINE PRIVATE VARIABLE Byggherre AS Controls.FillinChar NO-UNDO.
   DEFINE PRIVATE VARIABLE checkBoxAPersonFilter AS System.Windows.Forms.CheckBox NO-UNDO.
   DEFINE PRIVATE VARIABLE checkBoxPPeriodfilter AS System.Windows.Forms.CheckBox NO-UNDO.
   DEFINE PRIVATE VARIABLE comboSimpleLabel1 AS Controls.ComboSimpleLabel NO-UNDO.
   DEFINE PRIVATE VARIABLE comboSimpleLabelAAnvandare AS Controls.ComboSimpleLabel NO-UNDO.
   DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE dateTimeEditor1 AS Controls.DateTimeEditor NO-UNDO.
   DEFINE PRIVATE VARIABLE fillinChar2 AS Controls.FillinChar NO-UNDO.
   DEFINE PRIVATE VARIABLE fillinChar1 AS Controls.FillinChar NO-UNDO.
   DEFINE PRIVATE VARIABLE filterBox AS System.Windows.Forms.CheckBox NO-UNDO.
   DEFINE PRIVATE VARIABLE GPLSkapaMakuleraButton AS Infragistics.Win.Misc.UltraButton NO-UNDO.
   DEFINE PRIVATE VARIABLE GPLSkapaErsattButton AS Infragistics.Win.Misc.UltraButton NO-UNDO.
   DEFINE PRIVATE VARIABLE GridAktivitet AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE GridRattelser AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE Idnr AS Controls.FillinChar NO-UNDO.
   DEFINE PRIVATE VARIABLE popPanelMakulera AS Controls.PopPanel NO-UNDO.
   DEFINE PRIVATE VARIABLE popPanelErsatt AS Controls.PopPanel NO-UNDO.
   DEFINE PRIVATE VARIABLE ersattPop AS Infragistics.Win.Misc.UltraPopupControlContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE makuleraPop AS Infragistics.Win.Misc.UltraPopupControlContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE sparaXmlDialog AS System.Windows.Forms.SaveFileDialog NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerExportMMain AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerFilter AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerFilter2 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerInformationMain AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerExportMain AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerMain AS System.Windows.Forms.SplitContainer NO-UNDO.
   
   DEFINE PRIVATE VARIABLE HmtRubrikerlista             AS Controls.GridRubrikLista                                        NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC  VARIABLE ModuleHandle                 AS Guru.Module                                                    NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlDataDb                 AS Modules.Projekt.GPLDb                                      NO-UNDO.
   
   DEFINE PRIVATE VARIABLE splitContainerRattelserMain AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerStartMain AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE TabInformation AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabExportera AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabRattelser AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE textDatePStartDatum AS Controls.TextDate NO-UNDO.
   DEFINE PRIVATE VARIABLE textDatePSlutDatum AS Controls.TextDate NO-UNDO.
   DEFINE PUBLIC VARIABLE toolStrip1 AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE toolStripExport AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE toolStripButtonGeneraXML AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE toolStripSpara AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PRIVATE VARIABLE TabManager AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabAktivitet AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE Xkord AS Controls.FillinDecimal NO-UNDO.
   DEFINE PRIVATE VARIABLE Ykord AS Controls.FillinDecimal NO-UNDO.
   
   /*ribbon*/
   DEFINE PRIVATE VARIABLE RibbonTabVisa               AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   DEFINE PRIVATE VARIABLE RibbonTabInfo               AS Infragistics.Win.UltraWinToolbars.RibbonGroup                  NO-UNDO.
   
   
   /*Ribbon info*/
   DEFINE PUBLIC  VARIABLE RibbonToolAONR             AS Infragistics.Win.UltraWinToolbars.TextBoxTool                  NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolBNR             AS Infragistics.Win.UltraWinToolbars.TextBoxTool                  NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolAktiv            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolUppdatera            AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO.
   /*Ribbon inställningar*/
   DEFINE PUBLIC  VARIABLE RibbonToolDatum            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolStarttid            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolSluttid            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolFornamn            AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolEfternamn           AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolEnhet               AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   DEFINE PUBLIC  VARIABLE RibbonToolSkapadI               AS Infragistics.Win.UltraWinToolbars.StateButtonTool              NO-UNDO.
   /*ribbon slut*/
   
   
   DEFINE VARIABLE nyttaoapph AS HANDLE NO-UNDO.
   DEFINE VARIABLE blag AS CHARACTER NO-UNDO.
   DEFINE VARIABLE bolagorg AS CHARACTER NO-UNDO.
   
   DEFINE PRIVATE  VARIABLE senastUpd AS CHARACTER NO-UNDO.
   DEFINE PRIVATE  VARIABLE senastUpdDt AS DATETIME NO-UNDO.
   
    
   DEFINE PUBLIC PROPERTY Pidvar AS INTEGER  NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   DEFINE PUBLIC PROPERTY Aonrvar AS CHARACTER NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   DEFINE PUBLIC PROPERTY Delnrvar AS INTEGER  NO-UNDO
      PUBLIC GET.
      PUBLIC SET.

   /*   
   {GPLDYNTABLEH.I}
   */
   {GPLTTH.I}
   {GPLEXTRA.I}
   {GPLEXPORT.I}
   
   CONSTRUCTOR PUBLIC GPL (INPUT r AS Guru.Root):

      SUPER().
      THIS-OBJECT:ROOT = r.
      InitializeComponent ( ).
      THIS-OBJECT:Root:DatabaseManager:GPL:ControllGPL = THIS-OBJECT.
      THIS-OBJECT:HmtRubrikerlista = NEW Controls.GridRubrikLista().
      THIS-OBJECT:ControlDataDb = THIS-OBJECT:Root:DatabaseManager:GPL.
      THIS-OBJECT:ControlDataDb:InitializeTTs().
     
      IF THIS-OBJECT:ControlDataDb:GPLStart() = FALSE THEN RETURN.
      senastUpdDt = NOW.
      senastUpd = STRING(senastUpdDt, "99-99-9999 HH:MM:SS").
   
      {GPLDBH.I ControlDataDb }
    
      THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

   END CONSTRUCTOR.
   



   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID checkBoxAPersonFilter_CheckStateChanged ( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      IF THIS-OBJECT:checkBoxAPersonFilter:CHECKED THEN DO: 
         THIS-OBJECT:comboSimpleLabelAAnvandare:Visible = TRUE.
      END.
      ELSE DO:
         THIS-OBJECT:comboSimpleLabelAAnvandare:Visible = FALSE.
      END.
      RETURN.

   END METHOD.



   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID checkBoxPeriodFilter_CheckStateChanged ( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      IF THIS-OBJECT:checkBoxPPeriodFilter:CHECKED THEN DO:   
         THIS-OBJECT:textDatePStartDatum:Visible = TRUE.
         THIS-OBJECT:textDatePSlutDatum:Visible = TRUE.
      END.
      ELSE DO:
         THIS-OBJECT:textDatePStartDatum:Visible = FALSE.
         THIS-OBJECT:textDatePSlutDatum:Visible = FALSE.
      END.
      RETURN.

   END METHOD.


   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID filterBox_CheckStateChanged ( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      IF THIS-OBJECT:filterBox:CHECKED THEN DO: 
       THIS-OBJECT:splitContainerFilter2:Panel1Collapsed = FALSE.
      END.
      ELSE DO:
       THIS-OBJECT:splitContainerFilter2:Panel1Collapsed = TRUE.   
      END.
      
      RETURN.

   END METHOD.



    METHOD PUBLIC LOGICAL Initialize():
       DEBUGGER:SET-BREAK().
         
      THIS-OBJECT:CreateForm().
      
      /*THIS-OBJECT:TabManager:Tabs["Export"]:VISIBLE   = FALSE.*/
      IF Guru.konstanter:persekvar[1] = TRUE THEN DO:
         THIS-OBJECT:TabManager:Tabs["Export"]:VISIBLE   = TRUE.
      END.
      RETURN TRUE.
   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID AktivBox_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      IF THIS-OBJECT:RibbonToolAktiv:Checked THEN DO:
         THIS-OBJECT:ControlDataDb:aktiveraGPL(INPUT THIS-OBJECT:ControlDataDb:gplhuvudtth:BUFFER-FIELD("PLID"):BUFFER-VALUE, TRUE).
         System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(204)).
      END.
      ELSE DO:
         THIS-OBJECT:ControlDataDb:aktiveraGPL(INPUT THIS-OBJECT:ControlDataDb:gplhuvudtth:BUFFER-FIELD("PLID"):BUFFER-VALUE, FALSE).
         System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(205)).
      END.
      RETURN.
   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID RibbonToolDatum_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      IF THIS-OBJECT:RibbonToolDatum:Checked THEN DO:
         THIS-OBJECT:GridAktivitet:DisplayLayout:Bands[0]:Columns["LOGGDATUM"]:Hidden = FALSE.
      END.
      ELSE DO:
         THIS-OBJECT:GridAktivitet:DisplayLayout:Bands[0]:Columns["LOGGDATUM"]:Hidden = TRUE.
      END.
      RETURN.
   END METHOD.
   
      /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID RibbonToolStarttid_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
       IF THIS-OBJECT:RibbonToolStarttid:Checked THEN DO:
         THIS-OBJECT:GridAktivitet:DisplayLayout:Bands[0]:Columns["STARTTID"]:Hidden = FALSE.
      END.
      ELSE DO:
         THIS-OBJECT:GridAktivitet:DisplayLayout:Bands[0]:Columns["STARTTID"]:Hidden = TRUE.
      END.
      RETURN.
   END METHOD.
   
    /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID RibbonToolSluttid_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      IF THIS-OBJECT:RibbonToolSluttid:Checked THEN DO:
         THIS-OBJECT:GridAktivitet:DisplayLayout:Bands[0]:Columns["SLUTTID"]:Hidden = FALSE.
      END.
      ELSE DO:
         THIS-OBJECT:GridAktivitet:DisplayLayout:Bands[0]:Columns["SLUTTID"]:Hidden = TRUE.
      END.
      RETURN.
   END METHOD.
   
     /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID RibbonToolFornamn_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      IF THIS-OBJECT:RibbonToolFornamn:Checked THEN DO:
         THIS-OBJECT:GridAktivitet:DisplayLayout:Bands[0]:Columns["FORNAMN"]:Hidden = FALSE.
      END.
      ELSE DO:
         THIS-OBJECT:GridAktivitet:DisplayLayout:Bands[0]:Columns["FORNAMN"]:Hidden = TRUE.
      END.
      RETURN.
   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID RibbonToolEfternamn_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      IF THIS-OBJECT:RibbonToolEfternamn:Checked THEN DO:
         THIS-OBJECT:GridAktivitet:DisplayLayout:Bands[0]:Columns["EFTERNAMN"]:Hidden = FALSE.
      END.
      ELSE DO:
         THIS-OBJECT:GridAktivitet:DisplayLayout:Bands[0]:Columns["EFTERNAMN"]:Hidden = TRUE.
      END.
      RETURN.
   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID RibbonToolEnhet_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      IF THIS-OBJECT:RibbonToolEnhet:Checked THEN DO:
         THIS-OBJECT:GridAktivitet:DisplayLayout:Bands[0]:Columns["PERSONALKOD"]:Hidden = FALSE.
      END.
      ELSE DO:
         THIS-OBJECT:GridAktivitet:DisplayLayout:Bands[0]:Columns["PERSONALKOD"]:Hidden = TRUE.
      END.
      RETURN.
   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID RibbonToolSkapadI_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      IF THIS-OBJECT:RibbonToolSkapadI:Checked THEN DO:
         THIS-OBJECT:GridAktivitet:DisplayLayout:Bands[0]:Columns["SKAPADI"]:Hidden = FALSE.
      END.
      ELSE DO:
         THIS-OBJECT:GridAktivitet:DisplayLayout:Bands[0]:Columns["SKAPADI"]:Hidden = TRUE.
      END.
      RETURN.
   END METHOD.
   
   

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonCancelRattelse_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ersattPop:Close().
   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonCancelMakulera_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
       THIS-OBJECT:makuleraPop:Close().
      RETURN.

   END METHOD.

    METHOD PUBLIC VOID CreateForm():
      THIS-OBJECT:CreateGrids().
      THIS-OBJECT:CreateRibbon().
      
      THIS-OBJECT:LoadFields().
      
   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GPLSkapaMakuleraButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:MakuleraRattelseSkap().
      RETURN.

   END METHOD.

   METHOD PUBLIC VOID LoadFields():

      IF Guru.Konstanter:appcon THEN DO:           
         RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
      END.
      ELSE DO:
          RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph.
      END.
   
      THIS-OBJECT:Idnr:GuruLabel:Text = "Bygg-ID".
      THIS-OBJECT:Idnr:GuruText:Text = "vad du vill".
      THIS-OBJECT:Byggherre:GuruLabel:Text = "Byggherre".
      THIS-OBJECT:Borgnr:GuruLabel:Text = "Byggherre orgnr".
   /*      THIS-OBJECT:Borgnr:format("999999-9999").*/
      THIS-OBJECT:Xkord:GuruLabel:Text = "X-koordinat".
      THIS-OBJECT:Ykord:GuruLabel:Text = "Y-koordinat".
   
      THIS-OBJECT:ControlDataDb:gplaktivitettth:FIND-FIRST() NO-ERROR.
      IF THIS-OBJECT:ControlDataDb:gplaktivitettth:AVAILABLE THEN DO:
         THIS-OBJECT:textDatePStartDatum:VALUE = DATE(THIS-OBJECT:ControlDataDb:gplaktivitettth:BUFFER-FIELD("LOGGTID"):BUFFER-VALUE).
      END.
      THIS-OBJECT:ControlDataDb:gplaktivitettth:FIND-LAST() NO-ERROR.
      IF THIS-OBJECT:ControlDataDb:gplaktivitettth:AVAILABLE THEN DO:
         THIS-OBJECT:textDatePSlutDatum:VALUE = DATE(THIS-OBJECT:ControlDataDb:gplaktivitettth:BUFFER-FIELD("LOGGTID"):BUFFER-VALUE).
      END.
      THIS-OBJECT:ControlDataDb:gplhuvudtth:FIND-FIRST() NO-ERROR.
      IF THIS-OBJECT:ControlDataDb:gplhuvudtth:AVAILABLE THEN DO:
         THIS-OBJECT:RibbonToolAONR:Text = STRING(THIS-OBJECT:ControlDataDb:gplhuvudtth:BUFFER-FIELD("AONRAONR"):BUFFER-VALUE) + " " + STRING(THIS-OBJECT:ControlDataDb:gplhuvudtth:BUFFER-FIELD("AONRDELNR"):BUFFER-VALUE,Guru.Konstanter:varforetypchar[1]).
         THIS-OBJECT:Idnr:GuruText:Text = THIS-OBJECT:ControlDataDb:gplhuvudtth:BUFFER-FIELD("IDNR"):BUFFER-VALUE.
         RUN jurphmt_UI IN nyttaoapph (INPUT THIS-OBJECT:ControlDataDb:gplhuvudtth:BUFFER-FIELD("AONRAONR"):BUFFER-VALUE,INPUT THIS-OBJECT:ControlDataDb:gplhuvudtth:BUFFER-FIELD("AONRDELNR"):BUFFER-VALUE,OUTPUT blag,OUTPUT bolagorg).
         IF THIS-OBJECT:ControlDataDb:gplhuvudtth:BUFFER-FIELD("BYGGHERRENAMN"):BUFFER-VALUE = "" THEN DO:
            THIS-OBJECT:Byggherre:GuruText:Text = blag.
            THIS-OBJECT:Borgnr:GuruText:Text = bolagorg.
         END.
         ELSE DO:            
            THIS-OBJECT:Byggherre:GuruText:Text = THIS-OBJECT:ControlDataDb:gplhuvudtth:BUFFER-FIELD("BYGGHERRENAMN"):BUFFER-VALUE.
            THIS-OBJECT:Borgnr:GuruText:Text = THIS-OBJECT:ControlDataDb:gplhuvudtth:BUFFER-FIELD("BYGGHERREORGNR"):BUFFER-VALUE.
         END.  
         
         IF THIS-OBJECT:ControlDataDb:gplhuvudtth:BUFFER-FIELD("AKTIV"):BUFFER-VALUE = TRUE THEN DO:
            THIS-OBJECT:RibbonToolAktiv:InitializeChecked(TRUE).
         END.   
         ELSE THIS-OBJECT:RibbonToolAktiv:InitializeChecked(FALSE).
      END.
      
      THIS-OBJECT:splitContainerFilter2:Panel1Collapsed = TRUE.
      
      THIS-OBJECT:CreateComboExportAnvandare().
      
   END METHOD.
   
   METHOD PUBLIC VOID CreateGrids():
      THIS-OBJECT:CreateGridAktivitet(). 
   END METHOD.
   
   METHOD PUBLIC VOID CreateComboExportAnvandare():
      DEFINE VARIABLE anvlist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE anvAnv       AS Controls.GridRubrik      NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar          AS CHARACTER NO-UNDO.
      
      THIS-OBJECT:comboSimpleLabelAAnvandare:GuruLabel:Text        = "".

      queryvar =  "FOR EACH " + THIS-OBJECT:ControlDataDb:gplaktivitettth:TABLE + " BREAK BY PERSONALKOD".
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:gplaktivitettth,queryvar).
      
      EMPTY TEMP-TABLE gplexportanvandarett.
      
      DO WHILE qH:QUERY-OFF-END = FALSE:
         IF qH:FIRST-OF(1) = TRUE THEN DO:  
           CREATE gplexportanvandarett.
           ASSIGN 
              gplexportanvandarett.PERSONALKOD = THIS-OBJECT:ControlDataDb:gplaktivitettth:BUFFER-FIELD("PERSONALKOD"):BUFFER-VALUE.  
              gplexportanvandarett.NAMN = THIS-OBJECT:ControlDataDb:gplaktivitettth:BUFFER-FIELD("FORNAMN"):BUFFER-VALUE + " " + THIS-OBJECT:ControlDataDb:gplaktivitettth:BUFFER-FIELD("EFTERNAMN"):BUFFER-VALUE.
              gplexportanvandarett.PERSONNUMMER = THIS-OBJECT:ControlDataDb:gplaktivitettth:BUFFER-FIELD("PERSONNUMMER"):BUFFER-VALUE.      
         END.
         qH:GET-NEXT().
      END.


      anvlist = NEW Controls.GridRubrikLista().
      anvlist:ttBufferHandle = TEMP-TABLE gplexportanvandarett:DEFAULT-BUFFER-HANDLE.
      anvAnv = NEW Controls.GridRubrik().
      anvAnv:Falt = "PERSONALKOD".
      anvAnv:Rubrik = "PERSONALKOD".
      anvlist:addRubrik(anvAnv).      
      
      THIS-OBJECT:comboSimpleLabelAAnvandare:GuruCombo:GuruInitCombo(anvlist:ttBufferHandle, "PERSONALKOD", "NAMN").
      
   END METHOD.
   
   
   METHOD PUBLIC VOID CreateRibbon():
      THIS-OBJECT:ModuleHandle = THIS-OBJECT:Root:ModuleManager:GetModule("GPL").
      THIS-OBJECT:RibbonTabInfo = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Personalliggare").
      THIS-OBJECT:RibbonTabVisa = THIS-OBJECT:ModuleHandle:RibbonContext:Groups:Add("Visa").
      
      THIS-OBJECT:RibbonToolAONR = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("InfoAONR").
      THIS-OBJECT:RibbonToolAONR:SharedPropsInternal:Caption = "Projekt".
      THIS-OBJECT:RibbonToolAONR:Locked = TRUE.
         
      THIS-OBJECT:RibbonToolBNR = NEW Infragistics.Win.UltraWinToolbars.TextBoxTool("InfoByggNr").
      THIS-OBJECT:RibbonToolBNR:SharedPropsInternal:Caption = "Bygg ID".
      
      THIS-OBJECT:RibbonToolAktiv = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Aktiv").
      THIS-OBJECT:RibbonToolAktiv:SharedPropsInternal:Caption = "Aktiv".
      THIS-OBJECT:RibbonToolAktiv:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.

      
      THIS-OBJECT:RibbonToolDatum = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Datum").
      THIS-OBJECT:RibbonToolDatum:SharedPropsInternal:Caption = "Datum".
      THIS-OBJECT:RibbonToolDatum:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      THIS-OBJECT:RibbonToolStarttid = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Starttid").
      THIS-OBJECT:RibbonToolStarttid:SharedPropsInternal:Caption = "Starttid".
      THIS-OBJECT:RibbonToolStarttid:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      THIS-OBJECT:RibbonToolSluttid = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Sluttid").
      THIS-OBJECT:RibbonToolSluttid:SharedPropsInternal:Caption = "Sluttid".
      THIS-OBJECT:RibbonToolSluttid:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      THIS-OBJECT:RibbonToolFornamn = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Fornamn").
      THIS-OBJECT:RibbonToolFornamn:SharedPropsInternal:Caption = "Förnamn".
      THIS-OBJECT:RibbonToolFornamn:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      THIS-OBJECT:RibbonToolEfternamn = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Efternamn").
      THIS-OBJECT:RibbonToolEfternamn:SharedPropsInternal:Caption = "Efternamn".
      THIS-OBJECT:RibbonToolEfternamn:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      THIS-OBJECT:RibbonToolEnhet = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("Enhet").
      THIS-OBJECT:RibbonToolEnhet:SharedPropsInternal:Caption = "Enhet/Signatur".
      THIS-OBJECT:RibbonToolEnhet:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      THIS-OBJECT:RibbonToolSkapadI = NEW Infragistics.Win.UltraWinToolbars.StateButtonTool("SkapadI").
      THIS-OBJECT:RibbonToolSkapadI:SharedPropsInternal:Caption = "Skapad I".
      THIS-OBJECT:RibbonToolSkapadI:ToolbarDisplayStyle = Infragistics.Win.UltraWinToolbars.StateButtonToolbarDisplayStyle:Glyph.
      
      THIS-OBJECT:RibbonToolUppdatera = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Uppdatera").
      THIS-OBJECT:RibbonToolUppdatera:SharedPropsInternal:Caption = "Hämta senaste data".
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 1,INPUT THIS-OBJECT:RibbonToolUppdatera,INPUT "bilder/reloadgpl2.png").
      THIS-OBJECT:Root:ModuleManager:RibbonImage(INPUT 2,INPUT THIS-OBJECT:RibbonToolUppdatera,INPUT "bilder/reloadgpl2.png").
      THIS-OBJECT:RibbonToolUppdatera:SharedPropsInternal:Visible = TRUE.
      

      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolAONR,THIS-OBJECT:RibbonTabInfo).
     /* THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolBNR,THIS-OBJECT:RibbonTabInfo).*/
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolAktiv,THIS-OBJECT:RibbonTabInfo).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolUppdatera,THIS-OBJECT:RibbonTabInfo).
      
      
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolDatum,THIS-OBJECT:RibbonTabVisa).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolStarttid,THIS-OBJECT:RibbonTabVisa).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSluttid,THIS-OBJECT:RibbonTabVisa).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolFornamn,THIS-OBJECT:RibbonTabVisa).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolEfternamn,THIS-OBJECT:RibbonTabVisa).
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolEnhet,THIS-OBJECT:RibbonTabVisa).   
      THIS-OBJECT:Root:WindowManager:PrepRibbonTool(THIS-OBJECT:RibbonToolSkapadI,THIS-OBJECT:RibbonTabVisa).  
      
      THIS-OBJECT:RibbonToolUppdatera:ToolClick:SUBSCRIBE(THIS-OBJECT:GPLUppdbutton_Click).
      
      THIS-OBJECT:RibbonToolAktiv:ToolClick:SUBSCRIBE(AktivBox_Click).
      
      THIS-OBJECT:RibbonToolDatum:ToolClick:SUBSCRIBE(RibbonToolDatum_Click).
      THIS-OBJECT:RibbonToolStarttid:ToolClick:SUBSCRIBE(RibbonToolStarttid_Click).
      THIS-OBJECT:RibbonToolSluttid:ToolClick:SUBSCRIBE(RibbonToolSluttid_Click).
      THIS-OBJECT:RibbonToolFornamn:ToolClick:SUBSCRIBE(RibbonToolFornamn_Click).
      THIS-OBJECT:RibbonToolEfternamn:ToolClick:SUBSCRIBE(RibbonToolEfternamn_Click).
      THIS-OBJECT:RibbonToolEnhet:ToolClick:SUBSCRIBE(RibbonToolEnhet_Click).
      THIS-OBJECT:RibbonToolSkapadI:ToolClick:SUBSCRIBE(RibbonToolSkapadI_Click).
      
      THIS-OBJECT:RibbonToolDatum:InitializeChecked(TRUE).
      THIS-OBJECT:RibbonToolStarttid:InitializeChecked(TRUE).
      THIS-OBJECT:RibbonToolSluttid:InitializeChecked(TRUE).
      THIS-OBJECT:RibbonToolFornamn:InitializeChecked(TRUE).
      THIS-OBJECT:RibbonToolEfternamn:InitializeChecked(TRUE).
      THIS-OBJECT:RibbonToolEnhet:InitializeChecked(TRUE).
      
   END METHOD.
   
   METHOD PRIVATE VOID GPLUppdbutton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      
      THIS-OBJECT:ControlDataDb:laddaOmGPL().
      senastUpdDt = NOW.
      senastUpd = STRING(senastUpdDt, "99-99-9999 HH:MM:SS").
      /*
      MESSAGE "uppd"
      VIEW-AS ALERT-BOX.
      
      THIS-OBJECT:ControlDataDb:gplaktivitettth:FIND-LAST("WHERE PLID = 5 USE-INDEX PLAID").
      MESSAGE THIS-OBJECT:ControlDataDb:gplaktivitettth:BUFFER-FIELD("TYP"):BUFFER-VALUE
      VIEW-AS ALERT-BOX.
       
      */
      THIS-OBJECT:reCreateGridAktivitet().
      THIS-OBJECT:LoadFields().
      RETURN.
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridAktivitet():      
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("PLID","PLID",TRUE,30).
      rubrikergrid[2] = NEW Controls.GridRubrik("TYP","Typ",TRUE,30).
      rubrikergrid[3] = NEW Controls.GridRubrik("LOGGDATUM","Datum",TRUE,100,"DATE").
      rubrikergrid[4] = NEW Controls.GridRubrik("STARTTID","Startid",TRUE,100, "TIME").
      rubrikergrid[5] = NEW Controls.GridRubrik("SLUTTID","Sluttid",TRUE,100, "TIME").
      rubrikergrid[6] = NEW Controls.GridRubrik("PERSONALKOD","Enhet/Sign",TRUE,75).
      rubrikergrid[7] = NEW Controls.GridRubrik("FORNAMN","Förnamn",TRUE,60).
      rubrikergrid[8] = NEW Controls.GridRubrik("EFTERNAMN","Efternamn",TRUE,80).
      /*rubrikergrid[8] = NEW Controls.GridRubrik("ANVANDARE","Guruanvändare",TRUE,75).   
      rubrikergrid[9] = NEW Controls.GridRubrik("SKAPADAV","Skapad av",TRUE,75).*/
      rubrikergrid[9] = NEW Controls.GridRubrik("SKAPADI","Skapad i",TRUE,60).
      /*rubrikergrid[10] = NEW Controls.GridRubrik("RATTELSETYP","Åtgärd",TRUE,75).*/
      /*rubrikergrid[11] = NEW Controls.GridRubrik("PLAID","ID",TRUE,30).*/
      /*rubrikergrid[12] = NEW Controls.GridRubrik("RPLAID","Rättar",TRUE,40).*/
      
   
      THIS-OBJECT:GridAktivitet:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 12, INPUT THIS-OBJECT:ControlDataDb:gplaktivitettth)).
      
      THIS-OBJECT:GridAktivitet:DisplayLayout:Bands[0]:Columns["PLID"]:Hidden = TRUE.
      THIS-OBJECT:GridAktivitet:DisplayLayout:Bands[0]:Columns["SKAPADI"]:Hidden = TRUE.
      
      THIS-OBJECT:GridAktivitet:SetViewStyleBand(TRUE).
      THIS-OBJECT:GridAktivitet:SetTitle("Aktivitetslogg").
      THIS-OBJECT:GridAktivitet:TEXT = "Hämtat: " + senastUpd.
      THIS-OBJECT:GridAktivitet:GuruUpdateTitle().

      THIS-OBJECT:GridAktivitet:GuruOrderbyDefault(" BY LOGGTID DESCENDING").
      THIS-OBJECT:GridAktivitet:GuruFiltrera("RATTELSE = FALSE").
      
      /*THIS-OBJECT:GridAktivitet:GuruReOpen().*/
      /*
      THIS-OBJECT:GridAktivitet:GuruGridFarg(TRUE).             /*OBSE CASESENSITIVE*/ 
      THIS-OBJECT:GridAktivitet:GuruAddConditionalColor("RATTELSE", Elpool.Helpers:CT_NOTEQUALS, "false", Guru.StaticMethods:DotNetColor(15)).
      THIS-OBJECT:GridAktivitet:GuruAddConditionalColor("RATTELSETYP", Elpool.Helpers:CT_EQUALS, "Makulerad", Guru.StaticMethods:DotNetColor(16)).
      */
      /* RÄTTELSE KNAPPAR   
      THIS-OBJECT:GridAktivitet:ExtramenuItemAdd("Ersätt aktivitet").
      THIS-OBJECT:GridAktivitet:ExtraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:ersattSkapClick).
      THIS-OBJECT:GridAktivitet:ExtramenuItemAdd("Makulera aktivitet").
      THIS-OBJECT:GridAktivitet:ExtraToolStripMenuItem:Click:Subscribe(THIS-OBJECT:makuleraSkapClick).
      */
   END METHOD.  
         

   METHOD PUBLIC VOID reCreateGridAktivitet():
      {GridRubrikListaVarExtent.i} 
      /* funkar
      THIS-OBJECT:GridAktivitet:ReInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 10, INPUT THIS-OBJECT:ControlDataDb:gplaktivitettth)).
      */
      THIS-OBJECT:GridAktivitet:GuruGridFarg(FALSE).
      THIS-OBJECT:GridAktivitet:ReInitGrid(THIS-OBJECT:ControlDataDb:gplaktivitettth).
      THIS-OBJECT:GridAktivitet:SetViewStyleBand(TRUE).
      THIS-OBJECT:GridAktivitet:TEXT = "Hämtat: " + senastUpd.
      THIS-OBJECT:GridAktivitet:GuruGridFarg(TRUE).
      THIS-OBJECT:GridAktivitet:GuruFiltrera("RATTELSE = FALSE").

      THIS-OBJECT:GridAktivitet:GuruReOpen().
   END METHOD.  
   
   METHOD PUBLIC VOID ersattSkapClick(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      THIS-OBJECT:ersattPop:Show().
   END METHOD.
   
   METHOD PUBLIC VOID makuleraSkapClick(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
      THIS-OBJECT:makuleraPop:Show().
   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GPLSkapaRattelseButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ErsattRattelseSkap().   
   END METHOD.
   
   
   METHOD PUBLIC VOID ErsattRattelseSkap():
      DEFINE VARIABLE kommentar AS CHARACTER NO-UNDO.
      THIS-OBJECT:gplrattelsetth:EMPTY-TEMP-TABLE() NO-ERROR. 
      THIS-OBJECT:gplrattelsetth:BUFFER-CREATE().
      THIS-OBJECT:ControlDataDb:gplaktivitettth:FIND-FIRST("WHERE PLAID = " + STRING(THIS-OBJECT:GridAktivitet:ActiveRow:Cells["PLAID"]:VALUE)).
      THIS-OBJECT:gplrattelsetth:BUFFER-COPY(THIS-OBJECT:ControlDataDb:gplaktivitettth).
      THIS-OBJECT:gplrattelsetth:BUFFER-FIELD("RPLAID"):BUFFER-VALUE = THIS-OBJECT:gplrattelsetth:BUFFER-FIELD("PLAID"):BUFFER-VALUE.
      THIS-OBJECT:gplrattelsetth:BUFFER-FIELD("PLAID"):BUFFER-VALUE = 0.
      THIS-OBJECT:gplrattelsetth:BUFFER-FIELD("RATTELSE"):BUFFER-VALUE = TRUE.
      THIS-OBJECT:gplrattelsetth:BUFFER-FIELD("RATTELSETYP"):BUFFER-VALUE = "Ersätter".
      THIS-OBJECT:gplrattelsetth:BUFFER-FIELD("RATTELSEUTFORARE"):BUFFER-VALUE = Guru.Konstanter:globanv.
      
      IF THIS-OBJECT:ControlDataDb:skapaRattelse() = FALSE THEN DO:
         THIS-OBJECT:GPLUppdbutton_Click(?,?).
      END.
      ELSE DO:
       MESSAGE ""
       VIEW-AS ALERT-BOX.   
      END.
   END METHOD.
   
   METHOD PUBLIC VOID MakuleraRattelseSkap():
      DEFINE VARIABLE kommentar AS CHARACTER NO-UNDO.
      THIS-OBJECT:gplrattelsetth:EMPTY-TEMP-TABLE() NO-ERROR. 
      THIS-OBJECT:gplrattelsetth:BUFFER-CREATE().
      THIS-OBJECT:ControlDataDb:gplaktivitettth:FIND-FIRST("WHERE PLAID = " + STRING(THIS-OBJECT:GridAktivitet:ActiveRow:Cells["PLAID"]:VALUE)).
      THIS-OBJECT:gplrattelsetth:BUFFER-COPY(THIS-OBJECT:ControlDataDb:gplaktivitettth).
      THIS-OBJECT:gplrattelsetth:BUFFER-FIELD("RPLAID"):BUFFER-VALUE = THIS-OBJECT:gplrattelsetth:BUFFER-FIELD("PLAID"):BUFFER-VALUE.
      THIS-OBJECT:gplrattelsetth:BUFFER-FIELD("PLAID"):BUFFER-VALUE = 0.
      THIS-OBJECT:gplrattelsetth:BUFFER-FIELD("RATTELSE"):BUFFER-VALUE = TRUE.
      THIS-OBJECT:gplrattelsetth:BUFFER-FIELD("RATTELSETYP"):BUFFER-VALUE = "Makulerar".
      THIS-OBJECT:gplrattelsetth:BUFFER-FIELD("RATTELSEUTFORARE"):BUFFER-VALUE = Guru.Konstanter:globanv.
      
      IF THIS-OBJECT:ControlDataDb:skapaRattelse() = FALSE THEN DO:
         THIS-OBJECT:GPLUppdbutton_Click(?,?).
      END.
      ELSE DO:
         MESSAGE ""
         VIEW-AS ALERT-BOX.     
      END.
   END METHOD.
  
   METHOD PUBLIC VOID GeneraXML():
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE cTargetType     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE cFile           AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lFormatted      AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE cEncoding       AS CHARACTER NO-UNDO.
      DEFINE VARIABLE cSchemaLocation AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lWriteSchema    AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE lMinSchema      AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE utfillong AS LONGCHAR NO-UNDO.
      
      DEFINE VARIABLE filNamn     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE tidPunkt     AS CHARACTER NO-UNDO.
      
      THIS-OBJECT:RensaXML().
      
      THIS-OBJECT:ControlDataDb:gplhuvudtth:FIND-FIRST() NO-ERROR.
      
      tidPunkt = STRING(senastUpdDt, "9999-99-99 HH:MM:SS").
      tidPunkt = REPLACE(tidPunkt,' ','_').
      tidPunkt = REPLACE(tidPunkt,':','').
      
      filNamn = THIS-OBJECT:Idnr:GuruText:Text + "_" + tidPunkt + ".xml".
      
      THIS-OBJECT:sparaXmlDialog:FileName = filNamn.
      WAIT-FOR THIS-OBJECT:sparaXmlDialog:ShowDialog().
      
      CREATE Metadata.
      ASSIGN
        Metadata.Verksamhetstyp = "Bygg".
          
      IF THIS-OBJECT:filterBox:CHECKED THEN DO:   
         CREATE FilterBygg.
         ASSIGN
           FilterBygg.Metadata_id = RECID(Metadata).
         
         IF THIS-OBJECT:checkBoxPPeriodfilter:CHECKED THEN DO: 
            CREATE Period.
            ASSIGN
              Period.FilterBygg_id = RECID(FilterBygg)
              Period.Fromm = DATETIME(THIS-OBJECT:textDatePStartDatum:Datum).
              Period.Tom = DATETIME(THIS-OBJECT:textDatePSlutDatum:Datum).
         END.
         
         IF THIS-OBJECT:checkBoxAPersonFilter:CHECKED THEN DO:
            TEMP-TABLE gplexportanvandarett:DEFAULT-BUFFER-HANDLE:FIND-FIRST("WHERE gplexportanvandarett.PERSONALKOD = '" + STRING(THIS-OBJECT:comboSimpleLabelAAnvandare:GuruCombo:VALUE) + "'").
            CREATE VerksamPerson.
                  ASSIGN
                     VerksamPerson.FilterBygg_id  = RECID(FilterBygg)
                     VerksamPerson.Namn = TEMP-TABLE gplexportanvandarett:DEFAULT-BUFFER-HANDLE:BUFFER-FIELD("NAMN"):BUFFER-VALUE.
                     VerksamPerson.Identitetsnummer = THIS-OBJECT:comboSimpleLabelAAnvandare:GuruCombo:ttBufferHandle:BUFFER-FIELD("PERSONNUMMER"):BUFFER-VALUE.
         END.
        /*   EJ i drift          
         CREATE Byggverksamhetsbedrivare.
               ASSIGN
                  Byggverksamhetsbedrivare.FilterBygg_id = RECID(FilterBygg)
                  Byggverksamhetsbedrivare.Namn = blag
                  Byggverksamhetsbedrivare.Organisationsnummer = bolagorg.           
         */
      END.       
      CREATE Skapad.
      ASSIGN
         Skapad.Metadata_id = RECID(Metadata)
         Skapad.TidpunktSkapad =  DATETIME(TODAY, MTIME )
         Skapad.SkapadAvMjukvara = "Guru"
         Skapad.Mjukvaruversion = "2.0".
          
      CREATE ArbetsplatsBygg.
      ASSIGN
       /* ArbetsplatsBygg.Identifikationsnummer = THIS-OBJECT:ControlDataDb:gplhuvudtth:BUFFER-FIELD("IDNR"):BUFFER-VALUE.*/
       ArbetsplatsBygg.Identifikationsnummer = THIS-OBJECT:Idnr:GuruText:Text.
       
      CREATE Byggherre.
      ASSIGN
        Byggherre.ArbetsplatsBygg_id = RECID(ArbetsplatsBygg).
        
      IF THIS-OBJECT:ControlDataDb:gplhuvudtth:BUFFER-FIELD("BYGGHERRENAMN"):BUFFER-VALUE = "" THEN DO:
         ASSIGN
            Byggherre.Namn = blag
            Byggherre.Organisationsnummer = bolagorg.
      END.
      ELSE DO:
         ASSIGN
           Byggherre.Namn = THIS-OBJECT:ControlDataDb:gplhuvudtth:BUFFER-FIELD("BYGGHERRENAMN"):BUFFER-VALUE
           Byggherre.Organisationsnummer = THIS-OBJECT:ControlDataDb:gplhuvudtth:BUFFER-FIELD("BYGGHERREORGNR"):BUFFER-VALUE.
      END.
           
      CREATE Aktivitetslogg.
      ASSIGN
         Aktivitetslogg.ArbetsplatsBygg_id = RECID(ArbetsplatsBygg).
         
      /*Filter*/
      IF THIS-OBJECT:filterBox:CHECKED THEN DO:
         queryvar =  "FOR EACH " + THIS-OBJECT:ControlDataDb:gplaktivitettth:TABLE + " WHERE ".
         
         /*Period + användare*/
         IF THIS-OBJECT:checkBoxAPersonFilter:CHECKED AND THIS-OBJECT:checkBoxPPeriodfilter:CHECKED THEN DO:
            queryvar = queryvar + " DATE(LOGGTID) >= " + STRING(THIS-OBJECT:textDatePStartDatum:Datum) + " AND DATE(LOGGTID) <= "+  STRING(THIS-OBJECT:textDatePSlutDatum:Datum) + 
            " AND PERSONALKOD = '" + STRING(THIS-OBJECT:comboSimpleLabelAAnvandare:GuruCombo:VALUE) + "'".
         END.
         ELSE DO:
            /*Period*/
            IF THIS-OBJECT:checkBoxPPeriodfilter:CHECKED THEN DO:
                queryvar = queryvar + " DATE(LOGGTID) >= " + STRING(THIS-OBJECT:textDatePStartDatum:Datum) + " AND DATE(LOGGTID) <= "+  STRING(THIS-OBJECT:textDatePSlutDatum:Datum).
            END.
            /*Specifik användare*/
            IF THIS-OBJECT:checkBoxAPersonFilter:CHECKED THEN DO:
               queryvar = queryvar + " PERSONALKOD = '" + STRING(THIS-OBJECT:comboSimpleLabelAAnvandare:GuruCombo:VALUE) + "'".
            END.
         END.  
      END.
      ELSE DO:  
         queryvar =  "FOR EACH " + THIS-OBJECT:ControlDataDb:gplaktivitettth:TABLE.
      END.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:ControlDataDb:gplaktivitettth,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         CREATE Aktivitet.
            ASSIGN
               Aktivitet.Aktivitetslogg_id = RECID(Aktivitetslogg)
               Aktivitet.AktivitetsID = THIS-OBJECT:ControlDataDb:gplaktivitettth:BUFFER-FIELD("PLAID"):BUFFER-VALUE
               Aktivitet.Loggtidpunkt = THIS-OBJECT:ControlDataDb:gplaktivitettth:BUFFER-FIELD("LOGGTID"):BUFFER-VALUE.
               Aktivitet.Kommentar = THIS-OBJECT:ControlDataDb:gplaktivitettth:BUFFER-FIELD("KOMMENTAR"):BUFFER-VALUE.
               
        CREATE Byggverksamhetsbedrivare.
            ASSIGN
               Byggverksamhetsbedrivare.FilterBygg_id = RECID(Aktivitet)
               Byggverksamhetsbedrivare.Namn = blag
               Byggverksamhetsbedrivare.Organisationsnummer = bolagorg.         
       /* CREATE Byggverksamhetsbedrivare.
            ASSIGN
               Byggverksamhetsbedrivare.Aktivitet_id = RECID(Aktivitet)
               Byggverksamhetsbedrivare.Namn = THIS-OBJECT:ControlDataDb:gplhuvudtth:BUFFER-FIELD("VERKBEDRIVARENAMN"):BUFFER-VALUE
               Byggverksamhetsbedrivare.Organisationsnummer = THIS-OBJECT:ControlDataDb:gplhuvudtth:BUFFER-FIELD("VERKBEDRIVAREORGNR"):BUFFER-VALUE. */
               
       CREATE VerksamPerson.
            ASSIGN
               VerksamPerson.Aktivitet_id = RECID(Aktivitet)
               VerksamPerson.Namn = THIS-OBJECT:ControlDataDb:gplaktivitettth:BUFFER-FIELD("FORNAMN"):BUFFER-VALUE + " " + THIS-OBJECT:ControlDataDb:gplaktivitettth:BUFFER-FIELD("EFTERNAMN"):BUFFER-VALUE.
               VerksamPerson.Identitetsnummer = THIS-OBJECT:ControlDataDb:gplaktivitettth:BUFFER-FIELD("PERSONNUMMER"):BUFFER-VALUE. 
               IF VerksamPerson.Identitetsnummer EQ "" THEN DO:
                  ASSIGN
                  VerksamPerson.Identitetsnummer = "0000".
               END.
       CREATE Arbetspass.
          ASSIGN
             Arbetspass.Aktivitet_id = RECID(Aktivitet).
       IF THIS-OBJECT:ControlDataDb:gplaktivitettth:BUFFER-FIELD("TYP"):BUFFER-VALUE EQ "IN" THEN DO:   
          ASSIGN
             Arbetspass.Starttidpunkt = THIS-OBJECT:ControlDataDb:gplaktivitettth:BUFFER-FIELD("STARTTID"):BUFFER-VALUE.
       END.
       ELSE IF THIS-OBJECT:ControlDataDb:gplaktivitettth:BUFFER-FIELD("TYP"):BUFFER-VALUE EQ "UT" THEN DO:
          ASSIGN
             Arbetspass.Sluttidpunkt = THIS-OBJECT:ControlDataDb:gplaktivitettth:BUFFER-FIELD("SLUTTID"):BUFFER-VALUE.
       END.
           
         /*RÄTTELSE*/      
         IF THIS-OBJECT:ControlDataDb:gplaktivitettth:BUFFER-FIELD("RATTELSE"):BUFFER-VALUE EQ TRUE THEN DO: 
            CREATE Rattelse.
            ASSIGN
               Rattelse.Aktivitet_id = RECID(Aktivitet)
               Rattelse.RattarAktivitet = INTEGER(THIS-OBJECT:ControlDataDb:gplaktivitettth:BUFFER-FIELD("RPLAID"):BUFFER-VALUE)
               Rattelse.Rattelsetyp = THIS-OBJECT:ControlDataDb:gplaktivitettth:BUFFER-FIELD("RATTELSETYP"):BUFFER-VALUE.
            CREATE UtfordAv.
            ASSIGN
               UtfordAv.Rattelse_id = RECID(Rattelse)
               UtfordAv.Namn = THIS-OBJECT:ControlDataDb:gplaktivitettth:BUFFER-FIELD("RNAMN"):BUFFER-VALUE 
               UtfordAv.Identitetsnummer = THIS-OBJECT:ControlDataDb:gplaktivitettth:BUFFER-FIELD("RPERSONNUMMER"):BUFFER-VALUE. 
         END.
         qH:GET-NEXT().         
      END. 
       
   /*ASSIGN
     cTargetType     = "file"
     cFile           = "C:\tester\GPLSkatt.xml"
     lFormatted      = TRUE
     cEncoding       = ?
     cSchemaLocation = ?
     lWriteSchema    = FALSE
     lMinSchema      = FALSE.    
       
   DATASET Personalliggare:WRITE-XML(cTargetType, cFile, lFormatted, cEncoding, cSchemaLocation, lWriteSchema, lMinSchema).*/
   
   DATASET Personalliggare:WRITE-XML("LONGCHAR", utfillong, TRUE, "UTF-8").
   utfillong = REPLACE(utfillong,'<pl:Starttidpunkt xsi:nil="true"/>','').
   utfillong = REPLACE(utfillong,'<pl:Sluttidpunkt xsi:nil="true"/>','').
   
   COPY-LOB FROM utfillong TO FILE THIS-OBJECT:sparaXmlDialog:FileName CONVERT TARGET CODEPAGE "UTF-8" NO-ERROR.
   
   END METHOD. 
   
   METHOD PUBLIC VOID RensaXML():

      EMPTY TEMP-TABLE Metadata NO-ERROR.      
      EMPTY TEMP-TABLE Skapad NO-ERROR.   
      EMPTY TEMP-TABLE Period NO-ERROR.
      EMPTY TEMP-TABLE FilterBygg NO-ERROR.     
      EMPTY TEMP-TABLE ArbetsplatsBygg NO-ERROR.       
      EMPTY TEMP-TABLE Byggherre NO-ERROR.     
      EMPTY TEMP-TABLE Aktivitetslogg NO-ERROR.     
      EMPTY TEMP-TABLE Aktivitet NO-ERROR.
      
      EMPTY TEMP-TABLE Rattelse NO-ERROR.      
      EMPTY TEMP-TABLE UtfordAv NO-ERROR.        
      EMPTY TEMP-TABLE Byggverksamhetsbedrivare NO-ERROR.       
      EMPTY TEMP-TABLE VerksamPerson NO-ERROR.     
      EMPTY TEMP-TABLE Arbetspass NO-ERROR.    
      
   END METHOD.  
   
   METHOD PRIVATE VOID InitializeComponent(  ):
      
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
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Projekt.GPL").
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
      DEFINE VARIABLE dateButton1 AS Infragistics.Win.UltraWinSchedule.CalendarCombo.DateButton NO-UNDO.
      dateButton1 = NEW Infragistics.Win.UltraWinSchedule.CalendarCombo.DateButton().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE dateButton2 AS Infragistics.Win.UltraWinSchedule.CalendarCombo.DateButton NO-UNDO.
      dateButton2 = NEW Infragistics.Win.UltraWinSchedule.CalendarCombo.DateButton().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance25 AS Infragistics.Win.Appearance NO-UNDO.
      appearance25 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab14 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab14 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance26 AS Infragistics.Win.Appearance NO-UNDO.
      appearance26 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance27 AS Infragistics.Win.Appearance NO-UNDO.
      appearance27 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab17 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab17 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance28 AS Infragistics.Win.Appearance NO-UNDO.
      appearance28 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance29 AS Infragistics.Win.Appearance NO-UNDO.
      appearance29 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab2 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab2 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance30 AS Infragistics.Win.Appearance NO-UNDO.
      appearance30 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance31 AS Infragistics.Win.Appearance NO-UNDO.
      appearance31 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab6 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab6 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance32 AS Infragistics.Win.Appearance NO-UNDO.
      appearance32 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance33 AS Infragistics.Win.Appearance NO-UNDO.
      appearance33 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance34 AS Infragistics.Win.Appearance NO-UNDO.
      appearance34 = NEW Infragistics.Win.Appearance().
      THIS-OBJECT:TabAktivitet = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerStartMain = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridAktivitet = NEW Controls.Grid().
      THIS-OBJECT:TabInformation = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerInformationMain = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:toolStrip1 = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:toolStripSpara = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:Ykord = NEW Controls.FillinDecimal().
      THIS-OBJECT:comboSimpleLabel1 = NEW Controls.ComboSimpleLabel().
      THIS-OBJECT:Xkord = NEW Controls.FillinDecimal().
      THIS-OBJECT:Idnr = NEW Controls.FillinChar().
      THIS-OBJECT:Borgnr = NEW Controls.FillinChar().
      THIS-OBJECT:Byggherre = NEW Controls.FillinChar().
      THIS-OBJECT:TabRattelser = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerRattelserMain = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridRattelser = NEW Controls.Grid().
      THIS-OBJECT:TabExportera = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerExportMMain = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:toolStripExport = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:toolStripButtonGeneraXML = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:splitContainerExportMain = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerFilter = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:filterBox = NEW System.Windows.Forms.CheckBox().
      THIS-OBJECT:splitContainerFilter2 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:checkBoxAPersonFilter = NEW System.Windows.Forms.CheckBox().
      THIS-OBJECT:checkBoxPPeriodfilter = NEW System.Windows.Forms.CheckBox().
      THIS-OBJECT:comboSimpleLabelAAnvandare = NEW Controls.ComboSimpleLabel().
      THIS-OBJECT:textDatePSlutDatum = NEW Controls.TextDate().
      THIS-OBJECT:textDatePStartDatum = NEW Controls.TextDate().
      THIS-OBJECT:popPanelMakulera = NEW Controls.PopPanel().
      THIS-OBJECT:buttonCancelMakulera = NEW Controls.Button().
      THIS-OBJECT:GPLSkapaMakuleraButton = NEW Infragistics.Win.Misc.UltraButton().
      THIS-OBJECT:popPanelErsatt = NEW Controls.PopPanel().
      THIS-OBJECT:fillinChar2 = NEW Controls.FillinChar().
      THIS-OBJECT:fillinChar1 = NEW Controls.FillinChar().
      THIS-OBJECT:dateTimeEditor1 = NEW Controls.DateTimeEditor().
      THIS-OBJECT:buttonCancelErsatt = NEW Controls.Button().
      THIS-OBJECT:GPLSkapaErsattButton = NEW Infragistics.Win.Misc.UltraButton().
      THIS-OBJECT:ersattPop = NEW Infragistics.Win.Misc.UltraPopupControlContainer(THIS-OBJECT:components).
      THIS-OBJECT:makuleraPop = NEW Infragistics.Win.Misc.UltraPopupControlContainer(THIS-OBJECT:components).
      THIS-OBJECT:TabManager = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:sparaXmlDialog = NEW System.Windows.Forms.SaveFileDialog().
      THIS-OBJECT:TabAktivitet:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerStartMain, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerStartMain:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerStartMain:SuspendLayout().
      CAST(THIS-OBJECT:GridAktivitet, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabInformation:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerInformationMain, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerInformationMain:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerInformationMain:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerInformationMain:SuspendLayout().
      THIS-OBJECT:toolStrip1:SuspendLayout().
      THIS-OBJECT:TabRattelser:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerRattelserMain, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerRattelserMain:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerRattelserMain:SuspendLayout().
      CAST(THIS-OBJECT:GridRattelser, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabExportera:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerExportMMain, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerExportMMain:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerExportMMain:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerExportMMain:SuspendLayout().
      THIS-OBJECT:toolStripExport:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerExportMain, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerExportMain:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerExportMain:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerFilter, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerFilter:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerFilter:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerFilter:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerFilter2, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerFilter2:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerFilter2:SuspendLayout().
      CAST(THIS-OBJECT:textDatePSlutDatum, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:textDatePStartDatum, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:popPanelMakulera:ClientArea:SuspendLayout().
      THIS-OBJECT:popPanelMakulera:SuspendLayout().
      THIS-OBJECT:popPanelErsatt:ClientArea:SuspendLayout().
      THIS-OBJECT:popPanelErsatt:SuspendLayout().
      CAST(THIS-OBJECT:dateTimeEditor1, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:TabManager, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabManager:SuspendLayout().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* TabAktivitet */
      /*  */
      THIS-OBJECT:TabAktivitet:Controls:Add(THIS-OBJECT:splitContainerStartMain).
      THIS-OBJECT:TabAktivitet:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabAktivitet:Name = "TabAktivitet".
      THIS-OBJECT:TabAktivitet:Size = NEW System.Drawing.Size(1180, 512).
      /*  */
      /* splitContainerStartMain */
      /*  */
      THIS-OBJECT:splitContainerStartMain:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerStartMain:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerStartMain:Name = "splitContainerStartMain".
      /*  */
      /* splitContainerStartMain.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerStartMain:Panel1:Controls:Add(THIS-OBJECT:GridAktivitet).
      THIS-OBJECT:splitContainerStartMain:Panel2Collapsed = TRUE.
      THIS-OBJECT:splitContainerStartMain:Size = NEW System.Drawing.Size(1180, 512).
      THIS-OBJECT:splitContainerStartMain:SplitterDistance = 751.
      THIS-OBJECT:splitContainerStartMain:TabIndex = 1.
      /*  */
      /* GridAktivitet */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridAktivitet:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:GridAktivitet:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridAktivitet:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridAktivitet:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridAktivitet:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:GridAktivitet:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridAktivitet:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:GridAktivitet:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridAktivitet:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridAktivitet:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridAktivitet:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridAktivitet:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:GridAktivitet:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridAktivitet:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridAktivitet:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridAktivitet:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:GridAktivitet:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridAktivitet:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridAktivitet:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:GridAktivitet:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:GridAktivitet:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridAktivitet:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridAktivitet:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:GridAktivitet:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridAktivitet:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:GridAktivitet:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridAktivitet:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridAktivitet:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridAktivitet:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridAktivitet:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridAktivitet:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridAktivitet:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridAktivitet:GuruContext = ?.
      THIS-OBJECT:GridAktivitet:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridAktivitet:Name = "GridAktivitet".
      THIS-OBJECT:GridAktivitet:Size = NEW System.Drawing.Size(1180, 512).
      THIS-OBJECT:GridAktivitet:TabIndex = 1.
      THIS-OBJECT:GridAktivitet:Text = "grid1".
      /*  */
      /* TabInformation */
      /*  */
      THIS-OBJECT:TabInformation:Controls:Add(THIS-OBJECT:splitContainerInformationMain).
      THIS-OBJECT:TabInformation:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabInformation:Name = "TabInformation".
      THIS-OBJECT:TabInformation:Size = NEW System.Drawing.Size(1180, 512).
      /*  */
      /* splitContainerInformationMain */
      /*  */
      THIS-OBJECT:splitContainerInformationMain:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerInformationMain:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerInformationMain:Name = "splitContainerInformationMain".
      THIS-OBJECT:splitContainerInformationMain:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerInformationMain.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerInformationMain:Panel1:Controls:Add(THIS-OBJECT:toolStrip1).
      /*  */
      /* splitContainerInformationMain.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerInformationMain:Panel2:Controls:Add(THIS-OBJECT:Ykord).
      THIS-OBJECT:splitContainerInformationMain:Panel2:Controls:Add(THIS-OBJECT:comboSimpleLabel1).
      THIS-OBJECT:splitContainerInformationMain:Panel2:Controls:Add(THIS-OBJECT:Xkord).
      THIS-OBJECT:splitContainerInformationMain:Panel2:Controls:Add(THIS-OBJECT:Idnr).
      THIS-OBJECT:splitContainerInformationMain:Panel2:Controls:Add(THIS-OBJECT:Borgnr).
      THIS-OBJECT:splitContainerInformationMain:Panel2:Controls:Add(THIS-OBJECT:Byggherre).
      THIS-OBJECT:splitContainerInformationMain:Size = NEW System.Drawing.Size(1180, 512).
      THIS-OBJECT:splitContainerInformationMain:SplitterDistance = 29.
      THIS-OBJECT:splitContainerInformationMain:TabIndex = 1.
      /*  */
      /* toolStrip1 */
      /*  */
      THIS-OBJECT:toolStrip1:Dock = System.Windows.Forms.DockStyle:Fill.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 1 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:toolStripSpara.
      THIS-OBJECT:toolStrip1:Items:AddRange(arrayvar0).
      THIS-OBJECT:toolStrip1:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:toolStrip1:Name = "toolStrip1".
      THIS-OBJECT:toolStrip1:Size = NEW System.Drawing.Size(1180, 29).
      THIS-OBJECT:toolStrip1:TabIndex = 2.
      THIS-OBJECT:toolStrip1:Text = "toolStrip1".
      /*  */
      /* toolStripSpara */
      /*  */
      THIS-OBJECT:toolStripSpara:Image = CAST(resources:GetObject("toolStripSpara.Image"), System.Drawing.Image).
      THIS-OBJECT:toolStripSpara:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:toolStripSpara:Name = "toolStripSpara".
      THIS-OBJECT:toolStripSpara:Size = NEW System.Drawing.Size(110, 26).
      THIS-OBJECT:toolStripSpara:Text = "Spara ändringar".
      THIS-OBJECT:toolStripSpara:Click:Subscribe(THIS-OBJECT:toolStripButtonSpara).
      /*  */
      /* Ykord */
      /*  */
      THIS-OBJECT:Ykord:Location = NEW System.Drawing.Point(46, 318).
      THIS-OBJECT:Ykord:Name = "Ykord".
      THIS-OBJECT:Ykord:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:Ykord:TabIndex = 12.
      THIS-OBJECT:Ykord:Visible = FALSE.
      /*  */
      /* comboSimpleLabel1 */
      /*  */
      THIS-OBJECT:comboSimpleLabel1:Location = NEW System.Drawing.Point(81, 146).
      THIS-OBJECT:comboSimpleLabel1:Name = "comboSimpleLabel1".
      THIS-OBJECT:comboSimpleLabel1:Size = NEW System.Drawing.Size(208, 28).
      THIS-OBJECT:comboSimpleLabel1:TabIndex = 7.
      THIS-OBJECT:comboSimpleLabel1:Visible = FALSE.
      /*  */
      /* Xkord */
      /*  */
      THIS-OBJECT:Xkord:Location = NEW System.Drawing.Point(49, 270).
      THIS-OBJECT:Xkord:Name = "Xkord".
      THIS-OBJECT:Xkord:Size = NEW System.Drawing.Size(237, 28).
      THIS-OBJECT:Xkord:TabIndex = 11.
      THIS-OBJECT:Xkord:Visible = FALSE.
      /*  */
      /* Idnr */
      /*  */
      THIS-OBJECT:Idnr:Location = NEW System.Drawing.Point(12, 26).
      THIS-OBJECT:Idnr:Name = "Idnr".
      THIS-OBJECT:Idnr:Rubrik = "GuruLabel".
      THIS-OBJECT:Idnr:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:Idnr:TabIndex = 8.
      /*  */
      /* Borgnr */
      /*  */
      THIS-OBJECT:Borgnr:Location = NEW System.Drawing.Point(49, 226).
      THIS-OBJECT:Borgnr:Name = "Borgnr".
      THIS-OBJECT:Borgnr:Rubrik = "GuruLabel".
      THIS-OBJECT:Borgnr:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:Borgnr:TabIndex = 10.
      THIS-OBJECT:Borgnr:Visible = FALSE.
      /*  */
      /* Byggherre */
      /*  */
      THIS-OBJECT:Byggherre:Location = NEW System.Drawing.Point(49, 180).
      THIS-OBJECT:Byggherre:Name = "Byggherre".
      THIS-OBJECT:Byggherre:Rubrik = "GuruLabel".
      THIS-OBJECT:Byggherre:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:Byggherre:TabIndex = 9.
      THIS-OBJECT:Byggherre:Visible = FALSE.
      /*  */
      /* TabRattelser */
      /*  */
      THIS-OBJECT:TabRattelser:Controls:Add(THIS-OBJECT:splitContainerRattelserMain).
      THIS-OBJECT:TabRattelser:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabRattelser:Name = "TabRattelser".
      THIS-OBJECT:TabRattelser:Size = NEW System.Drawing.Size(1180, 512).
      /*  */
      /* splitContainerRattelserMain */
      /*  */
      THIS-OBJECT:splitContainerRattelserMain:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerRattelserMain:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerRattelserMain:Name = "splitContainerRattelserMain".
      /*  */
      /* splitContainerRattelserMain.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerRattelserMain:Panel1:Controls:Add(THIS-OBJECT:GridRattelser).
      THIS-OBJECT:splitContainerRattelserMain:Panel2Collapsed = TRUE.
      THIS-OBJECT:splitContainerRattelserMain:Size = NEW System.Drawing.Size(1180, 512).
      THIS-OBJECT:splitContainerRattelserMain:SplitterDistance = 751.
      THIS-OBJECT:splitContainerRattelserMain:TabIndex = 2.
      /*  */
      /* GridRattelser */
      /*  */
      appearance13:BackColor = System.Drawing.SystemColors:Window.
      appearance13:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridRattelser:DisplayLayout:Appearance = appearance13.
      THIS-OBJECT:GridRattelser:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridRattelser:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance14:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance14:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance14:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance14:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridRattelser:DisplayLayout:GroupByBox:Appearance = appearance14.
      appearance15:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridRattelser:DisplayLayout:GroupByBox:BandLabelAppearance = appearance15.
      THIS-OBJECT:GridRattelser:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance16:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance16:BackColor2 = System.Drawing.SystemColors:Control.
      appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance16:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridRattelser:DisplayLayout:GroupByBox:PromptAppearance = appearance16.
      THIS-OBJECT:GridRattelser:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridRattelser:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridRattelser:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance17:BackColor = System.Drawing.SystemColors:Window.
      appearance17:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridRattelser:DisplayLayout:Override:ActiveCellAppearance = appearance17.
      appearance18:BackColor = System.Drawing.SystemColors:Highlight.
      appearance18:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridRattelser:DisplayLayout:Override:ActiveRowAppearance = appearance18.
      THIS-OBJECT:GridRattelser:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridRattelser:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance19:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridRattelser:DisplayLayout:Override:CardAreaAppearance = appearance19.
      appearance20:BorderColor = System.Drawing.Color:Silver.
      appearance20:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridRattelser:DisplayLayout:Override:CellAppearance = appearance20.
      THIS-OBJECT:GridRattelser:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridRattelser:DisplayLayout:Override:CellPadding = 0.
      appearance21:BackColor = System.Drawing.SystemColors:Control.
      appearance21:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance21:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance21:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance21:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridRattelser:DisplayLayout:Override:GroupByRowAppearance = appearance21.
      appearance22:TextHAlignAsString = "Left".
      THIS-OBJECT:GridRattelser:DisplayLayout:Override:HeaderAppearance = appearance22.
      THIS-OBJECT:GridRattelser:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridRattelser:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance23:BackColor = System.Drawing.SystemColors:Window.
      appearance23:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridRattelser:DisplayLayout:Override:RowAppearance = appearance23.
      THIS-OBJECT:GridRattelser:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance24:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridRattelser:DisplayLayout:Override:TemplateAddRowAppearance = appearance24.
      THIS-OBJECT:GridRattelser:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridRattelser:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridRattelser:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridRattelser:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridRattelser:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridRattelser:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridRattelser:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridRattelser:GuruContext = ?.
      THIS-OBJECT:GridRattelser:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridRattelser:Name = "GridRattelser".
      THIS-OBJECT:GridRattelser:Size = NEW System.Drawing.Size(1180, 512).
      THIS-OBJECT:GridRattelser:TabIndex = 1.
      THIS-OBJECT:GridRattelser:Text = "grid1".
      /*  */
      /* TabExportera */
      /*  */
      THIS-OBJECT:TabExportera:Controls:Add(THIS-OBJECT:splitContainerExportMMain).
      THIS-OBJECT:TabExportera:Location = NEW System.Drawing.Point(1, 1).
      THIS-OBJECT:TabExportera:Name = "TabExportera".
      THIS-OBJECT:TabExportera:Size = NEW System.Drawing.Size(1180, 512).
      /*  */
      /* splitContainerExportMMain */
      /*  */
      THIS-OBJECT:splitContainerExportMMain:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerExportMMain:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerExportMMain:Name = "splitContainerExportMMain".
      THIS-OBJECT:splitContainerExportMMain:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerExportMMain.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerExportMMain:Panel1:Controls:Add(THIS-OBJECT:toolStripExport).
      /*  */
      /* splitContainerExportMMain.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerExportMMain:Panel2:Controls:Add(THIS-OBJECT:splitContainerExportMain).
      THIS-OBJECT:splitContainerExportMMain:Size = NEW System.Drawing.Size(1180, 512).
      THIS-OBJECT:splitContainerExportMMain:SplitterDistance = 27.
      THIS-OBJECT:splitContainerExportMMain:TabIndex = 1.
      /*  */
      /* toolStripExport */
      /*  */
      THIS-OBJECT:toolStripExport:Dock = System.Windows.Forms.DockStyle:Fill.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS System.Windows.Forms.ToolStripItem EXTENT 1 NO-UNDO.
      arrayvar1[1] = THIS-OBJECT:toolStripButtonGeneraXML.
      THIS-OBJECT:toolStripExport:Items:AddRange(arrayvar1).
      THIS-OBJECT:toolStripExport:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:toolStripExport:Name = "toolStripExport".
      THIS-OBJECT:toolStripExport:Size = NEW System.Drawing.Size(1180, 27).
      THIS-OBJECT:toolStripExport:TabIndex = 1.
      THIS-OBJECT:toolStripExport:Text = "toolStrip1".
      /*  */
      /* toolStripButtonGeneraXML */
      /*  */
      THIS-OBJECT:toolStripButtonGeneraXML:Image = CAST(resources:GetObject("toolStripButtonGeneraXML.Image"), System.Drawing.Image).
      THIS-OBJECT:toolStripButtonGeneraXML:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:toolStripButtonGeneraXML:Name = "toolStripButtonGeneraXML".
      THIS-OBJECT:toolStripButtonGeneraXML:Size = NEW System.Drawing.Size(116, 24).
      THIS-OBJECT:toolStripButtonGeneraXML:Text = "Generera XML-fil".
      THIS-OBJECT:toolStripButtonGeneraXML:Click:Subscribe(THIS-OBJECT:toolStripButtonGeneraXML_Click).
      /*  */
      /* splitContainerExportMain */
      /*  */
      THIS-OBJECT:splitContainerExportMain:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerExportMain:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerExportMain:Name = "splitContainerExportMain".
      /*  */
      /* splitContainerExportMain.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerExportMain:Panel1:Controls:Add(THIS-OBJECT:splitContainerFilter).
      THIS-OBJECT:splitContainerExportMain:Size = NEW System.Drawing.Size(1180, 481).
      THIS-OBJECT:splitContainerExportMain:SplitterDistance = 622.
      THIS-OBJECT:splitContainerExportMain:TabIndex = 0.
      /*  */
      /* splitContainerFilter */
      /*  */
      THIS-OBJECT:splitContainerFilter:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerFilter:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerFilter:Name = "splitContainerFilter".
      THIS-OBJECT:splitContainerFilter:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerFilter.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerFilter:Panel1:Controls:Add(THIS-OBJECT:filterBox).
      /*  */
      /* splitContainerFilter.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerFilter:Panel2:Controls:Add(THIS-OBJECT:splitContainerFilter2).
      THIS-OBJECT:splitContainerFilter:Size = NEW System.Drawing.Size(622, 481).
      THIS-OBJECT:splitContainerFilter:SplitterDistance = 67.
      THIS-OBJECT:splitContainerFilter:TabIndex = 12.
      /*  */
      /* filterBox */
      /*  */
      THIS-OBJECT:filterBox:Location = NEW System.Drawing.Point(20, 23).
      THIS-OBJECT:filterBox:Name = "filterBox".
      THIS-OBJECT:filterBox:Size = NEW System.Drawing.Size(104, 24).
      THIS-OBJECT:filterBox:TabIndex = 11.
      THIS-OBJECT:filterBox:Text = "Använd filter".
      THIS-OBJECT:filterBox:UseCompatibleTextRendering = TRUE.
      THIS-OBJECT:filterBox:UseVisualStyleBackColor = TRUE.
      THIS-OBJECT:filterBox:CheckStateChanged:Subscribe(THIS-OBJECT:filterBox_CheckStateChanged).
      /*  */
      /* splitContainerFilter2 */
      /*  */
      THIS-OBJECT:splitContainerFilter2:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerFilter2:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerFilter2:Name = "splitContainerFilter2".
      THIS-OBJECT:splitContainerFilter2:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerFilter2.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerFilter2:Panel1:Controls:Add(THIS-OBJECT:checkBoxAPersonFilter).
      THIS-OBJECT:splitContainerFilter2:Panel1:Controls:Add(THIS-OBJECT:checkBoxPPeriodfilter).
      THIS-OBJECT:splitContainerFilter2:Panel1:Controls:Add(THIS-OBJECT:comboSimpleLabelAAnvandare).
      THIS-OBJECT:splitContainerFilter2:Panel1:Controls:Add(THIS-OBJECT:textDatePSlutDatum).
      THIS-OBJECT:splitContainerFilter2:Panel1:Controls:Add(THIS-OBJECT:textDatePStartDatum).
      THIS-OBJECT:splitContainerFilter2:Size = NEW System.Drawing.Size(622, 410).
      THIS-OBJECT:splitContainerFilter2:SplitterDistance = 294.
      THIS-OBJECT:splitContainerFilter2:TabIndex = 21.
      /*  */
      /* checkBoxAPersonFilter */
      /*  */
      THIS-OBJECT:checkBoxAPersonFilter:Location = NEW System.Drawing.Point(291, 15).
      THIS-OBJECT:checkBoxAPersonFilter:Name = "checkBoxAPersonFilter".
      THIS-OBJECT:checkBoxAPersonFilter:Size = NEW System.Drawing.Size(104, 24).
      THIS-OBJECT:checkBoxAPersonFilter:TabIndex = 18.
      THIS-OBJECT:checkBoxAPersonFilter:Text = "Specifik person".
      THIS-OBJECT:checkBoxAPersonFilter:UseCompatibleTextRendering = TRUE.
      THIS-OBJECT:checkBoxAPersonFilter:UseVisualStyleBackColor = TRUE.
      THIS-OBJECT:checkBoxAPersonFilter:CheckStateChanged:Subscribe(THIS-OBJECT:checkBoxAPersonFilter_CheckStateChanged).
      /*  */
      /* checkBoxPPeriodfilter */
      /*  */
      THIS-OBJECT:checkBoxPPeriodfilter:Location = NEW System.Drawing.Point(20, 13).
      THIS-OBJECT:checkBoxPPeriodfilter:Name = "checkBoxPPeriodfilter".
      THIS-OBJECT:checkBoxPPeriodfilter:Size = NEW System.Drawing.Size(104, 24).
      THIS-OBJECT:checkBoxPPeriodfilter:TabIndex = 17.
      THIS-OBJECT:checkBoxPPeriodfilter:Text = "Period".
      THIS-OBJECT:checkBoxPPeriodfilter:UseCompatibleTextRendering = TRUE.
      THIS-OBJECT:checkBoxPPeriodfilter:UseVisualStyleBackColor = TRUE.
      THIS-OBJECT:checkBoxPPeriodfilter:CheckStateChanged:Subscribe(THIS-OBJECT:checkBoxPeriodfilter_CheckStateChanged).
      /*  */
      /* comboSimpleLabelAAnvandare */
      /*  */
      THIS-OBJECT:comboSimpleLabelAAnvandare:Location = NEW System.Drawing.Point(172, 45).
      THIS-OBJECT:comboSimpleLabelAAnvandare:Name = "comboSimpleLabelAAnvandare".
      THIS-OBJECT:comboSimpleLabelAAnvandare:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:comboSimpleLabelAAnvandare:TabIndex = 14.
      THIS-OBJECT:comboSimpleLabelAAnvandare:Visible = FALSE.
      /*  */
      /* textDatePSlutDatum */
      /*  */
      THIS-OBJECT:textDatePSlutDatum:DateButtons:Add(dateButton1).
      THIS-OBJECT:textDatePSlutDatum:Datum = Progress.Util.DateTimeHelper:Create(2016, 8, 31, 0, 0, 0, 0).
      THIS-OBJECT:textDatePSlutDatum:Location = NEW System.Drawing.Point(20, 80).
      THIS-OBJECT:textDatePSlutDatum:Name = "textDatePSlutDatum".
      THIS-OBJECT:textDatePSlutDatum:NonAutoSizeHeight = 21.
      THIS-OBJECT:textDatePSlutDatum:Size = NEW System.Drawing.Size(121, 21).
      THIS-OBJECT:textDatePSlutDatum:TabIndex = 15.
      THIS-OBJECT:textDatePSlutDatum:Value = Progress.Util.DateTimeHelper:Create(2016, 8, 31, 0, 0, 0, 0).
      THIS-OBJECT:textDatePSlutDatum:Visible = FALSE.
      /*  */
      /* textDatePStartDatum */
      /*  */
      THIS-OBJECT:textDatePStartDatum:DateButtons:Add(dateButton2).
      THIS-OBJECT:textDatePStartDatum:Datum = Progress.Util.DateTimeHelper:Create(2016, 8, 31, 0, 0, 0, 0).
      THIS-OBJECT:textDatePStartDatum:Location = NEW System.Drawing.Point(20, 43).
      THIS-OBJECT:textDatePStartDatum:Name = "textDatePStartDatum".
      THIS-OBJECT:textDatePStartDatum:NonAutoSizeHeight = 21.
      THIS-OBJECT:textDatePStartDatum:Size = NEW System.Drawing.Size(121, 21).
      THIS-OBJECT:textDatePStartDatum:TabIndex = 16.
      THIS-OBJECT:textDatePStartDatum:Value = Progress.Util.DateTimeHelper:Create(2016, 8, 31, 0, 0, 0, 0).
      THIS-OBJECT:textDatePStartDatum:Visible = FALSE.
      /*  */
      /* popPanelMakulera */
      /*  */
      THIS-OBJECT:popPanelMakulera:BorderStyle = Infragistics.Win.UIElementBorderStyle:Rounded4.
      /*  */
      /* popPanelMakulera.ClientArea */
      /*  */
      THIS-OBJECT:popPanelMakulera:ClientArea:Controls:Add(THIS-OBJECT:buttonCancelMakulera).
      THIS-OBJECT:popPanelMakulera:ClientArea:Controls:Add(THIS-OBJECT:GPLSkapaMakuleraButton).
      THIS-OBJECT:popPanelMakulera:Location = NEW System.Drawing.Point(290, 0).
      THIS-OBJECT:popPanelMakulera:Name = "popPanelMakulera".
      THIS-OBJECT:popPanelMakulera:Size = NEW System.Drawing.Size(284, 238).
      THIS-OBJECT:popPanelMakulera:TabIndex = 9.
      THIS-OBJECT:popPanelMakulera:Visible = FALSE.
      /*  */
      /* buttonCancelMakulera */
      /*  */
      THIS-OBJECT:buttonCancelMakulera:Location = NEW System.Drawing.Point(203, 205).
      THIS-OBJECT:buttonCancelMakulera:Name = "buttonCancelMakulera".
      THIS-OBJECT:buttonCancelMakulera:Size = NEW System.Drawing.Size(75, 26).
      THIS-OBJECT:buttonCancelMakulera:TabIndex = 5.
      THIS-OBJECT:buttonCancelMakulera:Text = "Avbryt".
      THIS-OBJECT:buttonCancelMakulera:Click:Subscribe(THIS-OBJECT:buttonCancelMakulera_Click).
      /*  */
      /* GPLSkapaMakuleraButton */
      /*  */
      THIS-OBJECT:GPLSkapaMakuleraButton:Location = NEW System.Drawing.Point(73, 205).
      THIS-OBJECT:GPLSkapaMakuleraButton:Name = "GPLSkapaMakuleraButton".
      THIS-OBJECT:GPLSkapaMakuleraButton:Size = NEW System.Drawing.Size(130, 26).
      THIS-OBJECT:GPLSkapaMakuleraButton:TabIndex = 3.
      THIS-OBJECT:GPLSkapaMakuleraButton:Text = "Skapa rättelse".
      THIS-OBJECT:GPLSkapaMakuleraButton:Click:Subscribe(THIS-OBJECT:GPLSkapaMakuleraButton_Click).
      /*  */
      /* popPanelErsatt */
      /*  */
      THIS-OBJECT:popPanelErsatt:BorderStyle = Infragistics.Win.UIElementBorderStyle:Rounded4.
      /*  */
      /* popPanelErsatt.ClientArea */
      /*  */
      THIS-OBJECT:popPanelErsatt:ClientArea:Controls:Add(THIS-OBJECT:fillinChar2).
      THIS-OBJECT:popPanelErsatt:ClientArea:Controls:Add(THIS-OBJECT:fillinChar1).
      THIS-OBJECT:popPanelErsatt:ClientArea:Controls:Add(THIS-OBJECT:dateTimeEditor1).
      THIS-OBJECT:popPanelErsatt:ClientArea:Controls:Add(THIS-OBJECT:buttonCancelErsatt).
      THIS-OBJECT:popPanelErsatt:ClientArea:Controls:Add(THIS-OBJECT:GPLSkapaErsattButton).
      THIS-OBJECT:popPanelErsatt:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:popPanelErsatt:Name = "popPanelErsatt".
      THIS-OBJECT:popPanelErsatt:Size = NEW System.Drawing.Size(284, 238).
      THIS-OBJECT:popPanelErsatt:TabIndex = 8.
      THIS-OBJECT:popPanelErsatt:Visible = FALSE.
      /*  */
      /* fillinChar2 */
      /*  */
      THIS-OBJECT:fillinChar2:Location = NEW System.Drawing.Point(38, 66).
      THIS-OBJECT:fillinChar2:Name = "fillinChar2".
      THIS-OBJECT:fillinChar2:Rubrik = "GuruLabel".
      THIS-OBJECT:fillinChar2:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:fillinChar2:TabIndex = 8.
      /*  */
      /* fillinChar1 */
      /*  */
      THIS-OBJECT:fillinChar1:Location = NEW System.Drawing.Point(38, 32).
      THIS-OBJECT:fillinChar1:Name = "fillinChar1".
      THIS-OBJECT:fillinChar1:Rubrik = "GuruLabel".
      THIS-OBJECT:fillinChar1:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:fillinChar1:TabIndex = 7.
      /*  */
      /* dateTimeEditor1 */
      /*  */
      THIS-OBJECT:dateTimeEditor1:Location = NEW System.Drawing.Point(134, 127).
      THIS-OBJECT:dateTimeEditor1:MaskInput = "~{LOC~}yyyy/mm/dd hh:mm:ss tt".
      THIS-OBJECT:dateTimeEditor1:Name = "dateTimeEditor1".
      THIS-OBJECT:dateTimeEditor1:Size = NEW System.Drawing.Size(144, 21).
      THIS-OBJECT:dateTimeEditor1:TabIndex = 6.
      /*  */
      /* buttonCancelErsatt */
      /*  */
      THIS-OBJECT:buttonCancelErsatt:Location = NEW System.Drawing.Point(203, 205).
      THIS-OBJECT:buttonCancelErsatt:Name = "buttonCancelErsatt".
      THIS-OBJECT:buttonCancelErsatt:Size = NEW System.Drawing.Size(75, 26).
      THIS-OBJECT:buttonCancelErsatt:TabIndex = 5.
      THIS-OBJECT:buttonCancelErsatt:Text = "Avbryt".
      THIS-OBJECT:buttonCancelErsatt:Click:Subscribe(THIS-OBJECT:buttonCancelRattelse_Click).
      /*  */
      /* GPLSkapaErsattButton */
      /*  */
      THIS-OBJECT:GPLSkapaErsattButton:Location = NEW System.Drawing.Point(73, 205).
      THIS-OBJECT:GPLSkapaErsattButton:Name = "GPLSkapaErsattButton".
      THIS-OBJECT:GPLSkapaErsattButton:Size = NEW System.Drawing.Size(130, 26).
      THIS-OBJECT:GPLSkapaErsattButton:TabIndex = 3.
      THIS-OBJECT:GPLSkapaErsattButton:Text = "Skapa rättelse".
      THIS-OBJECT:GPLSkapaErsattButton:Click:Subscribe(THIS-OBJECT:GPLSkapaRattelseButton_Click).
      /*  */
      /* ersattPop */
      /*  */
      THIS-OBJECT:ersattPop:PopupControl = THIS-OBJECT:popPanelErsatt.
      /*  */
      /* makuleraPop */
      /*  */
      THIS-OBJECT:makuleraPop:PopupControl = THIS-OBJECT:popPanelMakulera.
      /*  */
      /* TabManager */
      /*  */
      appearance25:BackColor = System.Drawing.SystemColors:Control.
      THIS-OBJECT:TabManager:Appearance = appearance25.
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabRattelser).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabExportera).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabAktivitet).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabInformation).
      THIS-OBJECT:TabManager:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:TabManager:ImageSize = NEW System.Drawing.Size(40, 40).
      THIS-OBJECT:TabManager:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:TabManager:Name = "TabManager".
      THIS-OBJECT:TabManager:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
      THIS-OBJECT:TabManager:Size = NEW System.Drawing.Size(1184, 562).
      THIS-OBJECT:TabManager:TabIndex = 45.
      THIS-OBJECT:TabManager:TabOrientation = Infragistics.Win.UltraWinTabs.TabOrientation:BottomLeft.
      appearance26:Image = CAST(resources:GetObject("appearance26.Image"), System.Object).
      ultraTab14:ActiveAppearance = appearance26.
      appearance27:Image = CAST(resources:GetObject("appearance27.Image"), System.Object).
      ultraTab14:Appearance = appearance27.
      ultraTab14:Key = "Logg".
      ultraTab14:TabPage = THIS-OBJECT:TabAktivitet.
      ultraTab14:Text = "".
      ultraTab14:ToolTipText = "Personalliggare".
      appearance28:Image = CAST(resources:GetObject("appearance28.Image"), System.Object).
      ultraTab17:ActiveAppearance = appearance28.
      appearance29:Image = CAST(resources:GetObject("appearance29.Image"), System.Object).
      ultraTab17:Appearance = appearance29.
      ultraTab17:Key = "Information".
      ultraTab17:TabPage = THIS-OBJECT:TabInformation.
      ultraTab17:Text = "".
      ultraTab17:ToolTipText = "Information".
      appearance30:Image = CAST(resources:GetObject("appearance30.Image"), System.Object).
      ultraTab2:ActiveAppearance = appearance30.
      appearance31:Image = CAST(resources:GetObject("appearance31.Image"), System.Object).
      ultraTab2:Appearance = appearance31.
      ultraTab2:Key = "Rättelser".
      ultraTab2:TabPage = THIS-OBJECT:TabRattelser.
      ultraTab2:ToolTipText = "Rättelser".
      ultraTab2:Visible = FALSE.
      appearance32:Image = CAST(resources:GetObject("appearance32.Image"), System.Object).
      ultraTab6:ActiveAppearance = appearance32.
      appearance33:Image = CAST(resources:GetObject("appearance33.Image"), System.Object).
      ultraTab6:Appearance = appearance33.
      appearance34:Image = CAST(resources:GetObject("appearance34.Image"), System.Object).
      ultraTab6:ClientAreaAppearance = appearance34.
      ultraTab6:Key = "Export".
      ultraTab6:TabPage = THIS-OBJECT:TabExportera.
      ultraTab6:ToolTipText = "Exportera".
      ultraTab6:Visible = FALSE.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 4 NO-UNDO.
      arrayvar2[1] = ultraTab14.
      arrayvar2[2] = ultraTab17.
      arrayvar2[3] = ultraTab2.
      arrayvar2[4] = ultraTab6.
      THIS-OBJECT:TabManager:Tabs:AddRange(arrayvar2).
      THIS-OBJECT:TabManager:SelectedTabChanging:Subscribe(THIS-OBJECT:TabManager_SelectedTabChanging).
      /*  */
      /* ultraTabSharedControlsPage1 */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1".
      THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(1180, 512).
      /*  */
      /* sparaXmlDialog */
      /*  */
      THIS-OBJECT:sparaXmlDialog:FileOk:Subscribe(THIS-OBJECT:sparaXmlDialog_FileOk).
      /*  */
      /* GPL */
      /*  */
      THIS-OBJECT:AllowDrop = TRUE.
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:TabManager).
      THIS-OBJECT:Name = "GPL".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1184, 562).
      THIS-OBJECT:TabAktivitet:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerStartMain:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerStartMain, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerStartMain:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridAktivitet, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabInformation:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerInformationMain:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerInformationMain:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerInformationMain:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerInformationMain, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerInformationMain:ResumeLayout(FALSE).
      THIS-OBJECT:toolStrip1:ResumeLayout(FALSE).
      THIS-OBJECT:toolStrip1:PerformLayout().
      THIS-OBJECT:TabRattelser:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerRattelserMain:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerRattelserMain, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerRattelserMain:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridRattelser, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabExportera:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerExportMMain:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerExportMMain:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerExportMMain:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerExportMMain, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerExportMMain:ResumeLayout(FALSE).
      THIS-OBJECT:toolStripExport:ResumeLayout(FALSE).
      THIS-OBJECT:toolStripExport:PerformLayout().
      THIS-OBJECT:splitContainerExportMain:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerExportMain, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerExportMain:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerFilter:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerFilter:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerFilter, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerFilter:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerFilter2:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerFilter2:Panel1:PerformLayout().
      CAST(THIS-OBJECT:splitContainerFilter2, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerFilter2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:textDatePSlutDatum, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:textDatePStartDatum, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:popPanelMakulera:ClientArea:ResumeLayout(FALSE).
      THIS-OBJECT:popPanelMakulera:ResumeLayout(FALSE).
      THIS-OBJECT:popPanelErsatt:ClientArea:ResumeLayout(FALSE).
      THIS-OBJECT:popPanelErsatt:ClientArea:PerformLayout().
      THIS-OBJECT:popPanelErsatt:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:dateTimeEditor1, System.ComponentModel.ISupportInitialize):EndInit().
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
	METHOD PRIVATE VOID sparaXmlDialog_FileOk( INPUT sender AS System.Object, INPUT e AS System.ComponentModel.CancelEventArgs ):
		
		RETURN.

	END METHOD.


   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID TabManager_SelectedTabChanging( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangingEventArgs ):
      
      RETURN.

   END METHOD.


	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID toolStripButtonGeneraXML_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:GeneraXML().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID toolStripButtonSpara( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:ControlDataDb:gplhuvudtth:FIND-FIRST() NO-ERROR.
      IF THIS-OBJECT:ControlDataDb:gplhuvudtth:AVAILABLE THEN DO:
   		THIS-OBJECT:ControlDataDb:uppdateraByggID(INPUT THIS-OBJECT:ControlDataDb:gplhuvudtth:BUFFER-FIELD("PLID"):BUFFER-VALUE, INPUT THIS-OBJECT:Idnr:GuruText:Text).
      END.
		
		RETURN.

	END METHOD.

   DESTRUCTOR PUBLIC GPL ( ):
      
      DELETE OBJECT Root NO-ERROR.
      
      IF VALID-OBJECT(components) THEN DO:
         CAST(components, System.IDisposable):Dispose().
      END.
   END DESTRUCTOR.

END CLASS.