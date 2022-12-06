 
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
USING Progress.Windows.Form.



 
 /*------------------------------------------------------------------------
    File        : GPL
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Thu Apr 21 10:04:04 CEST 2016
    Notes       : 
  ----------------------------------------------------------------------*/
USING Progress.Windows.Form.



CLASS Modules.Projekt.GPL INHERITS Form: 
   
   DEFINE PRIVATE VARIABLE Borgnr AS Controls.FillinChar NO-UNDO.
   DEFINE PRIVATE VARIABLE Aktiv AS System.Windows.Forms.CheckBox NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonOK AS Infragistics.Win.Misc.UltraButton NO-UNDO.
   DEFINE PRIVATE VARIABLE Byggherre AS Controls.FillinChar NO-UNDO.
   DEFINE PRIVATE VARIABLE comboSimpleLabel1 AS Controls.ComboSimpleLabel NO-UNDO.
   DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE GPLXMLbutton AS Infragistics.Win.Misc.UltraButton NO-UNDO.
   DEFINE PRIVATE VARIABLE GPLOkButton AS Infragistics.Win.Misc.UltraButton NO-UNDO.
   DEFINE PRIVATE VARIABLE GridAktivitet AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE Idnr AS Controls.FillinChar NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerBot AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerMain AS System.Windows.Forms.SplitContainer NO-UNDO.
   
   DEFINE PRIVATE VARIABLE HmtRubrikerlista             AS Controls.GridRubrikLista                                        NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlDataDb                 AS Modules.Projekt.GPLDb                                      NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerStart AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerPlh AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerStartMain AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE Xkord AS Controls.FillinDecimal NO-UNDO.
   DEFINE VARIABLE nyttaoapph AS HANDLE NO-UNDO.
   DEFINE VARIABLE blag AS CHARACTER NO-UNDO.
   DEFINE VARIABLE bolagorg AS CHARACTER NO-UNDO.
   DEFINE PRIVATE VARIABLE Ykord AS Controls.FillinDecimal NO-UNDO.
    
   DEFINE PUBLIC PROPERTY Pidvar AS INTEGER  NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   DEFINE PUBLIC PROPERTY Aonrvar AS CHARACTER NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
   DEFINE PUBLIC PROPERTY Delnrvar AS INTEGER  NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
      
   {GPLDYNTABLEH.I}
   
   {GPLEXPORT.I}
   
   CONSTRUCTOR PUBLIC GPL (INPUT r AS Guru.Root):

      SUPER().
      THIS-OBJECT:ROOT = r.
      InitializeComponent ( ).
      THIS-OBJECT:Root:DatabaseManager:GPL:ControllGPL = THIS-OBJECT.
      THIS-OBJECT:HmtRubrikerlista = NEW Controls.GridRubrikLista().
      THIS-OBJECT:ControlDataDb = THIS-OBJECT:Root:DatabaseManager:GPL.
      THIS-OBJECT:ControlDataDb:InitializeTTs().
      {GPLDBH.I ControlDataDb }
      THIS-OBJECT:CreateForm().
      THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

   END CONSTRUCTOR.
   
    METHOD PUBLIC VOID CreateForm():
      THIS-OBJECT:Text = "Guru Personalliggare ".
      THIS-OBJECT:LoadFields().
      THIS-OBJECT:CreateGrids().
   END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

	METHOD PRIVATE VOID GPLXMLbutton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:GeneraXML().
		RETURN.

	END METHOD.

   METHOD PUBLIC VOID LoadFields():
      
      IF Guru.Konstanter:appcon THEN DO:           
         RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
      END.
      ELSE DO:
          RUN NYTTAOAPP.P PERSISTENT SET nyttaoapph.
      END.
   
   
   
      THIS-OBJECT:Idnr:GuruLabel:Text = "Idnr på byggarbetsplats".
      THIS-OBJECT:Idnr:GuruText:Text = "vad du vill".
      THIS-OBJECT:Byggherre:GuruLabel:Text = "Byggherre".
      THIS-OBJECT:Borgnr:GuruLabel:Text = "Byggherre orgnr".
/*      THIS-OBJECT:Borgnr:format("999999-9999").*/
      THIS-OBJECT:Xkord:GuruLabel:Text = "X-koordinat".
      THIS-OBJECT:Ykord:GuruLabel:Text = "Y-koordinat".
      
      DEBUGGER:SET-BREAK().
      THIS-OBJECT:gplhuvudtth:FIND-FIRST() NO-ERROR.
      IF THIS-OBJECT:gplhuvudtth:AVAILABLE THEN DO:
         THIS-OBJECT:Idnr:GuruText:Text = THIS-OBJECT:gplhuvudtth:BUFFER-FIELD("IDNR"):BUFFER-VALUE.
         RUN jurphmt_UI IN nyttaoapph (INPUT gplhuvudtth:BUFFER-FIELD("AONRAONR"):BUFFER-VALUE,INPUT gplhuvudtth:BUFFER-FIELD("AONRDELNR"):BUFFER-VALUE,OUTPUT blag,OUTPUT bolagorg).
         IF THIS-OBJECT:gplhuvudtth:BUFFER-FIELD("BYGGHERRENAMN"):BUFFER-VALUE = "" THEN DO:
            THIS-OBJECT:Byggherre:GuruText:Text = blag.
            THIS-OBJECT:Borgnr:GuruText:Text = bolagorg.
         END.
         ELSE DO:            
            THIS-OBJECT:Byggherre:GuruText:Text = THIS-OBJECT:gplhuvudtth:BUFFER-FIELD("BYGGHERRENAMN"):BUFFER-VALUE.
            THIS-OBJECT:Borgnr:GuruText:Text = THIS-OBJECT:gplhuvudtth:BUFFER-FIELD("BYGGHERREORGNR"):BUFFER-VALUE.
         END.  
         DEBUGGER:SET-BREAK().
         
         IF THIS-OBJECT:gplhuvudtth:BUFFER-FIELD("AKTIV"):BUFFER-VALUE = TRUE THEN DO:
            THIS-OBJECT:AKTIV:CHECKED  = TRUE.
         END.   
         ELSE THIS-OBJECT:AKTIV:CHECKED  = FALSE.
            
            
      END.   
      
   END METHOD.
   METHOD PUBLIC VOID CreateGrids():
      THIS-OBJECT:CreateGridAktivitet(). 
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridAktivitet():      
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("PLID","PLID",TRUE,30).
      rubrikergrid[2] = NEW Controls.GridRubrik("PLAID","PLAID",TRUE,30).
      rubrikergrid[3] = NEW Controls.GridRubrik("TYP","Typ",TRUE,30).
      rubrikergrid[4] = NEW Controls.GridRubrik("LOGGTID","Loggtid",TRUE,100,"DATE").
      rubrikergrid[5] = NEW Controls.GridRubrik("STARTTID","Startid",TRUE,100, "TIME").
      rubrikergrid[6] = NEW Controls.GridRubrik("SLUTTID","Sluttid",TRUE,100, "TIME").
      rubrikergrid[7] = NEW Controls.GridRubrik("ANVANDARE","Guruanvändare",TRUE,75).
      rubrikergrid[8] = NEW Controls.GridRubrik("PERSONALKOD","Personalkod",TRUE,75).
      rubrikergrid[9] = NEW Controls.GridRubrik("SKAPADAV","Skapad av",TRUE,75).
      rubrikergrid[10] = NEW Controls.GridRubrik("SKAPADI","Skapad i",TRUE,100).
      
   
      THIS-OBJECT:GridAktivitet:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 10, INPUT THIS-OBJECT:gplaktivitettth)).
      
      THIS-OBJECT:GridAktivitet:DisplayLayout:Bands[0]:Columns["PLID"]:Hidden = TRUE.
      THIS-OBJECT:GridAktivitet:DisplayLayout:Bands[0]:Columns["PLAID"]:Hidden = TRUE.
      THIS-OBJECT:GridAktivitet:SetViewStyleBand(TRUE).
      THIS-OBJECT:GridAktivitet:TEXT = "Aktivitetslogg".
      THIS-OBJECT:GridAktivitet:GuruReOpen().
      THIS-OBJECT:GridAktivitet:SetTitle("Aktivitetslogg").   
   END METHOD.  
   
   METHOD PUBLIC VOID GeneraXML():
      DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
      DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
         
      THIS-OBJECT:RensaXML().
      
      THIS-OBJECT:gplhuvudtth:FIND-FIRST() NO-ERROR.
      
      CREATE Metadata.
      ASSIGN
        Metadata.Verksamhetstyp = "Bygg".
        
      CREATE ArbetsplatsBygg.
      ASSIGN
        ArbetsplatsBygg.Identifikationsnummer = THIS-OBJECT:gplhuvudtth:BUFFER-FIELD("IDNR"):BUFFER-VALUE.
       
      CREATE Byggherre.
      ASSIGN
        Byggherre.ArbetsplatsBygg_id = RECID(ArbetsplatsBygg).
        Byggherre.Namn = THIS-OBJECT:gplhuvudtth:BUFFER-FIELD("BYGGHERRENAMN"):BUFFER-VALUE
        Byggherre.Organisationsnummer = THIS-OBJECT:gplhuvudtth:BUFFER-FIELD("BYGGHERREORGNR"):BUFFER-VALUE
        
          
      CREATE Aktivitetslogg.
      ASSIGN
         Aktivitetslogg.ArbetsplatsBygg_id = RECID(ArbetsplatsBygg).
        
      queryvar =  "FOR EACH " + THIS-OBJECT:gplaktivitettth:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:gplaktivitettth,queryvar).
      qH:GET-FIRST().

      DO WHILE qH:QUERY-OFF-END = FALSE:
         CREATE Aktivitet.
            ASSIGN
               Aktivitet.Aktivitetslogg_id = RECID(Aktivitetslogg)
               Aktivitet.AktivitetsID = THIS-OBJECT:gplaktivitettth:BUFFER-FIELD("PLAID"):BUFFER-VALUE
               Aktivitet.Loggtidpunkt = THIS-OBJECT:gplaktivitettth:BUFFER-FIELD("LOGGTID"):BUFFER-VALUE
               Aktivitet.Kommentar = THIS-OBJECT:gplaktivitettth:BUFFER-FIELD("KOMMENTAR"):BUFFER-VALUE.
               
        CREATE Byggverksamhetsbedrivare.
            ASSIGN
               Byggverksamhetsbedrivare.Aktivitets_id = RECID(Aktivitet)
               Byggverksamhetsbedrivare.Namn = THIS-OBJECT:gplhuvudtth:BUFFER-FIELD("VERKBEDRIVARENAMN"):BUFFER-VALUE
               Byggverksamhetsbedrivare.Organisationsnummer = THIS-OBJECT:gplhuvudtth:BUFFER-FIELD("VERKBEDRIVAREORGNR"):BUFFER-VALUE. 
               
       CREATE VerksamPerson.
            ASSIGN
               VerksamPerson.Aktivitets_id = RECID(Aktivitet)
               VerksamPerson.Namn = THIS-OBJECT:gplaktivitettth:BUFFER-FIELD("PERSONALKOD"):BUFFER-VALUE /*ÄNDRA FRÅN ALAIS TILL REALNAME*/
               VerksamPerson.Identitetsnummer = THIS-OBJECT:gplaktivitettth:BUFFER-FIELD("PERSONALKOD"):BUFFER-VALUE. /*ÄNDRA FRÅN ALAIS TILL PERSONNUMMER*/
               
       CREATE Arbetspass.
            ASSIGN
               Arbetspass.Aktivitets_id = RECID(Aktivitet)
               Arbetspass.Sluttidpunkt = THIS-OBJECT:gplaktivitettth:BUFFER-FIELD("STARTTID"):BUFFER-VALUE 
               Arbetspass.Starttidpunkt = THIS-OBJECT:gplaktivitettth:BUFFER-FIELD("SLUTTID"):BUFFER-VALUE. 
          
         /*RÄTTELSE*/      
         IF THIS-OBJECT:gplaktivitettth:BUFFER-FIELD("RATTALSE"):BUFFER-VALUE = TRUE THEN DO:
            CREATE Rattelse.
            ASSIGN
               Aktivitet.Aktivitet_id = RECID(Aktivitet)
               Aktivitet.RattarAktivitet = INTEGER(THIS-OBJECT:gplaktivitettth:BUFFER-FIELD("RPLAID"):BUFFER-VALUE)
               Aktivitet.Rattelsetyp = THIS-OBJECT:gplaktivitettth:BUFFER-FIELD("RATTALSETYP"):BUFFER-VALUE.
            CREATE UtfordAv.
            ASSIGN
               UtfordAv.Rattelse = RECID(Rattelse)
               UfordAv.Namn = THIS-OBJECT:gplaktivitettth:BUFFER-FIELD("RATTALSEUTFORARE"):BUFFER-VALUE /*ÄNDRA FRÅN ALAIS TILL REALNAME*/
               UfordAv.Namn = 1. /*PERSONNUMMER*/
               .

         
         qH:GET-NEXT().         
      END. 
       
       
       
   DATASET Personalliggare:WRITE-XML("FILE", "C:\tester\GPLskatt.xml").
   
   END METHOD. 
   METHOD PUBLIC VOID RensaXML():

      EMPTY TEMP-TABLE Metadata NO-ERROR.      
      EMPTY TEMP-TABLE Skapad NO-ERROR.        
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
      resources = NEW Progress.Util.ResourceManager("Modules.Projekt.GPL").
      THIS-OBJECT:splitContainerStart = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerStartMain = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerPlh = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:Aktiv = NEW System.Windows.Forms.CheckBox().
      THIS-OBJECT:Ykord = NEW Controls.FillinDecimal().
      THIS-OBJECT:Xkord = NEW Controls.FillinDecimal().
      THIS-OBJECT:Borgnr = NEW Controls.FillinChar().
      THIS-OBJECT:Byggherre = NEW Controls.FillinChar().
      THIS-OBJECT:Idnr = NEW Controls.FillinChar().
      THIS-OBJECT:comboSimpleLabel1 = NEW Controls.ComboSimpleLabel().
      THIS-OBJECT:GPLXMLbutton = NEW Infragistics.Win.Misc.UltraButton().
      THIS-OBJECT:GridAktivitet = NEW Controls.Grid().
      THIS-OBJECT:splitContainerBot = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GPLOkButton = NEW Infragistics.Win.Misc.UltraButton().
      CAST(THIS-OBJECT:splitContainerStart, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerStart:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerStart:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerStart:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerStartMain, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerStartMain:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerStartMain:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerStartMain:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerPlh, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerPlh:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerPlh:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerPlh:SuspendLayout().
      CAST(THIS-OBJECT:GridAktivitet, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerBot, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerBot:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerBot:SuspendLayout().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainerStart */
      /*  */
      THIS-OBJECT:splitContainerStart:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerStart:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerStart:Name = "splitContainerStart".
      THIS-OBJECT:splitContainerStart:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerStart.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerStart:Panel1:Controls:Add(THIS-OBJECT:splitContainerStartMain).
      /*  */
      /* splitContainerStart.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerStart:Panel2:Controls:Add(THIS-OBJECT:splitContainerBot).
      THIS-OBJECT:splitContainerStart:Size = NEW System.Drawing.Size(1184, 562).
      THIS-OBJECT:splitContainerStart:SplitterDistance = 513.
      THIS-OBJECT:splitContainerStart:TabIndex = 0.
      /*  */
      /* splitContainerStartMain */
      /*  */
      THIS-OBJECT:splitContainerStartMain:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerStartMain:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerStartMain:Name = "splitContainerStartMain".
      /*  */
      /* splitContainerStartMain.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerStartMain:Panel1:Controls:Add(THIS-OBJECT:splitContainerPlh).
      /*  */
      /* splitContainerStartMain.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerStartMain:Panel2:Controls:Add(THIS-OBJECT:GridAktivitet).
      THIS-OBJECT:splitContainerStartMain:Size = NEW System.Drawing.Size(1184, 513).
      THIS-OBJECT:splitContainerStartMain:SplitterDistance = 394.
      THIS-OBJECT:splitContainerStartMain:TabIndex = 1.
      /*  */
      /* splitContainerPlh */
      /*  */
      THIS-OBJECT:splitContainerPlh:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerPlh:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerPlh:Name = "splitContainerPlh".
      THIS-OBJECT:splitContainerPlh:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerPlh.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerPlh:Panel1:Controls:Add(THIS-OBJECT:Aktiv).
      THIS-OBJECT:splitContainerPlh:Panel1:Controls:Add(THIS-OBJECT:Ykord).
      THIS-OBJECT:splitContainerPlh:Panel1:Controls:Add(THIS-OBJECT:Xkord).
      THIS-OBJECT:splitContainerPlh:Panel1:Controls:Add(THIS-OBJECT:Borgnr).
      THIS-OBJECT:splitContainerPlh:Panel1:Controls:Add(THIS-OBJECT:Byggherre).
      THIS-OBJECT:splitContainerPlh:Panel1:Controls:Add(THIS-OBJECT:Idnr).
      THIS-OBJECT:splitContainerPlh:Panel1:Controls:Add(THIS-OBJECT:comboSimpleLabel1).
      /*  */
      /* splitContainerPlh.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerPlh:Panel2:Controls:Add(THIS-OBJECT:GPLXMLbutton).
      THIS-OBJECT:splitContainerPlh:Size = NEW System.Drawing.Size(394, 513).
      THIS-OBJECT:splitContainerPlh:SplitterDistance = 256.
      THIS-OBJECT:splitContainerPlh:TabIndex = 1.
      /*  */
      /* Aktiv */
      /*  */
      THIS-OBJECT:Aktiv:Location = NEW System.Drawing.Point(143, 49).
      THIS-OBJECT:Aktiv:Name = "Aktiv".
      THIS-OBJECT:Aktiv:Size = NEW System.Drawing.Size(104, 24).
      THIS-OBJECT:Aktiv:TabIndex = 6.
      THIS-OBJECT:Aktiv:Text = "Aktiv".
      THIS-OBJECT:Aktiv:UseCompatibleTextRendering = TRUE.
      THIS-OBJECT:Aktiv:UseVisualStyleBackColor = TRUE.
      /*  */
      /* Ykord */
      /*  */
      THIS-OBJECT:Ykord:Location = NEW System.Drawing.Point(60, 213).
      THIS-OBJECT:Ykord:Name = "Ykord".
      THIS-OBJECT:Ykord:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:Ykord:TabIndex = 5.
      /*  */
      /* Xkord */
      /*  */
      THIS-OBJECT:Xkord:Location = NEW System.Drawing.Point(60, 179).
      THIS-OBJECT:Xkord:Name = "Xkord".
      THIS-OBJECT:Xkord:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:Xkord:TabIndex = 4.
      /*  */
      /* Borgnr */
      /*  */
      THIS-OBJECT:Borgnr:Location = NEW System.Drawing.Point(60, 145).
      THIS-OBJECT:Borgnr:Name = "Borgnr".
      THIS-OBJECT:Borgnr:Rubrik = "GuruLabel".
      THIS-OBJECT:Borgnr:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:Borgnr:TabIndex = 3.
      /*  */
      /* Byggherre */
      /*  */
      THIS-OBJECT:Byggherre:Location = NEW System.Drawing.Point(60, 111).
      THIS-OBJECT:Byggherre:Name = "Byggherre".
      THIS-OBJECT:Byggherre:Rubrik = "GuruLabel".
      THIS-OBJECT:Byggherre:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:Byggherre:TabIndex = 2.
      /*  */
      /* Idnr */
      /*  */
      THIS-OBJECT:Idnr:Location = NEW System.Drawing.Point(60, 77).
      THIS-OBJECT:Idnr:Name = "Idnr".
      THIS-OBJECT:Idnr:Rubrik = "GuruLabel".
      THIS-OBJECT:Idnr:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:Idnr:TabIndex = 1.
      /*  */
      /* comboSimpleLabel1 */
      /*  */
      THIS-OBJECT:comboSimpleLabel1:Location = NEW System.Drawing.Point(60, 15).
      THIS-OBJECT:comboSimpleLabel1:Name = "comboSimpleLabel1".
      THIS-OBJECT:comboSimpleLabel1:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:comboSimpleLabel1:TabIndex = 0.
      /*  */
      /* GPLXMLbutton */
      /*  */
      THIS-OBJECT:GPLXMLbutton:Location = NEW System.Drawing.Point(143, 16).
      THIS-OBJECT:GPLXMLbutton:Name = "GPLXMLbutton".
      THIS-OBJECT:GPLXMLbutton:Size = NEW System.Drawing.Size(130, 25).
      THIS-OBJECT:GPLXMLbutton:TabIndex = 2.
      THIS-OBJECT:GPLXMLbutton:Text = "Genera XML-fil".
      THIS-OBJECT:GPLXMLbutton:Click:Subscribe(THIS-OBJECT:GPLXMLbutton_Click).
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
      THIS-OBJECT:GridAktivitet:Size = NEW System.Drawing.Size(786, 513).
      THIS-OBJECT:GridAktivitet:TabIndex = 1.
      THIS-OBJECT:GridAktivitet:Text = "grid1".
      /*  */
      /* splitContainerBot */
      /*  */
      THIS-OBJECT:splitContainerBot:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerBot:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerBot:Name = "splitContainerBot".
      /*  */
      /* splitContainerBot.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerBot:Panel2:Controls:Add(THIS-OBJECT:GPLOkButton).
      THIS-OBJECT:splitContainerBot:Size = NEW System.Drawing.Size(1184, 45).
      THIS-OBJECT:splitContainerBot:SplitterDistance = 394.
      THIS-OBJECT:splitContainerBot:TabIndex = 0.
      /*  */
      /* GPLOkButton */
      /*  */
      appearance13:Image = CAST(resources:GetObject("appearance13.Image"), System.Object).
      THIS-OBJECT:GPLOkButton:Appearance = appearance13.
      THIS-OBJECT:GPLOkButton:Location = NEW System.Drawing.Point(680, 10).
      THIS-OBJECT:GPLOkButton:Name = "GPLOkButton".
      THIS-OBJECT:GPLOkButton:Size = NEW System.Drawing.Size(100, 25).
      THIS-OBJECT:GPLOkButton:TabIndex = 0.
      THIS-OBJECT:GPLOkButton:Text = "Ok".
      /*  */
      /* GPL */
      /*  */
      THIS-OBJECT:ClientSize = NEW System.Drawing.Size(1184, 562).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerStart).
      THIS-OBJECT:Name = "GPL".
      THIS-OBJECT:Text = "GPL".
      THIS-OBJECT:splitContainerStart:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerStart:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerStart, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerStart:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerStartMain:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerStartMain:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerStartMain, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerStartMain:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerPlh:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerPlh:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerPlh, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerPlh:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridAktivitet, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerBot:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerBot, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerBot:ResumeLayout(FALSE).
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.

	DESTRUCTOR PUBLIC GPL ( ):

	END DESTRUCTOR.

END CLASS.