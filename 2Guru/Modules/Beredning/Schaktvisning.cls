 
 /*------------------------------------------------------------------------
    File        : Schaktvisning
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Thu Sep 29 13:18:10 CEST 2016
    Notes       : 
  ----------------------------------------------------------------------*/
USING Progress.Windows.Form.
USING Progress.Lang.*.



CLASS Modules.Beredning.Schaktvisning INHERITS Form   : 
   {HDTEMP.i}
   DEFINE TEMP-TABLE uftfort NO-UNDO
   FIELD UTYP AS INTEGER
   FIELD TYPC AS CHARACTER LABEL "Visa".  
   DEFINE PRIVATE VARIABLE ButtonOK AS Infragistics.Win.Misc.UltraButton NO-UNDO.
   DEFINE PUBLIC VARIABLE CmbUtf AS Controls.ComboSimple NO-UNDO.
   DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE gridSchaktProt AS Controls.GridJoin NO-UNDO.
   DEFINE VARIABLE antalDStabeller AS INTEGER NO-UNDO.
   DEFINE VARIABLE hdschakttempbuffh AS HANDLE NO-UNDO.
   DEFINE VARIABLE hdschakprottempbuffh AS HANDLE EXTENT 30 NO-UNDO.
   DEFINE VARIABLE hdprotkopbertempbuffh AS HANDLE NO-UNDO.
   DEFINE VARIABLE hdschakprothandtempbuffh AS HANDLE NO-UNDO.
   DEFINE VARIABLE hdkalktempbuffh AS HANDLE NO-UNDO.
   DEFINE PUBLIC VARIABLE LabelKatalog AS Infragistics.Win.Misc.UltraLabel NO-UNDO.
   DEFINE VARIABLE schkordstartsluttempbuffh AS HANDLE NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerVal AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE VARIABLE sumkbuffh AS HANDLE NO-UNDO.
   DEFINE VARIABLE topbuffh AS HANDLE NO-UNDO.
   
   DEFINE VARIABLE antalProtTabeller AS INTEGER NO-UNDO.
   
   DEFINE PRIVATE VARIABLE splitContainerMain AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE HmtRubrikerlistaJoin AS Controls.GridRubrikListaJoin NO-UNDO.
   {GridRubrikListaJoinVarExtent.i}
   DEFINE VARIABLE SchaktProtDSh AS HANDLE NO-UNDO.
   
   CONSTRUCTOR PUBLIC Schaktvisning ():
       
      SUPER().
                 
      InitializeComponent().
      
      THIS-OBJECT:SIZE = NEW System.Drawing.Size(Guru.Konstanter:globstorb, Guru.Konstanter:globstorh).
      
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
      
   END CONSTRUCTOR.
   


	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonOK_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:Close().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID CmbUtf_ValueChanged( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
    
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID gridSchaktProt_VisibleChanged( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      IF Guru.GlobalaVariabler:fonstercol > 0 THEN THIS-OBJECT:LEFT = Guru.GlobalaVariabler:fonstercol.
      IF Guru.GlobalaVariabler:fonsterrow > 0 THEN THIS-OBJECT:TOP = Guru.GlobalaVariabler:fonsterrow.
      THIS-OBJECT:GridSchaktProt:GuruExpand(TRUE).
		RETURN.
	END METHOD.

   METHOD PUBLIC VOID InitiateSchaktVis():
      THIS-OBJECT:CreateCmbUtf().
      THIS-OBJECT:CreateGridSchaktProt().
      
      RETURN.
   END METHOD.
   
   METHOD PUBLIC VOID ConnectDataSet(DATASET-HANDLE inSchaktProtDSh BIND, antalDStabeller AS INTEGER):
      DEFINE VARIABLE iCount AS INTEGER NO-UNDO INITIAL 1.
      DEFINE VARIABLE iCount2 AS INTEGER NO-UNDO INITIAL 1.
      
      THIS-OBJECT:SchaktProtDSh:EMPTY-DATASET() NO-ERROR.
      
      THIS-OBJECT:antalProtTabeller = antalDStabeller - 7.
      antalDStabeller = THIS-OBJECT:antalProtTabeller.
      
      IF VALID-HANDLE(inSchaktProtDSh) THEN DO:
         topbuffh = inSchaktProtDSh:GET-BUFFER-HANDLE(iCount).
         iCount = iCount + 1.
         hdschakttempbuffh = inSchaktProtDSh:GET-BUFFER-HANDLE(iCount).
         iCount = iCount + 1.
         DO WHILE antalDStabeller NE 0:
            hdschakprottempbuffh[iCount2] = inSchaktProtDSh:GET-BUFFER-HANDLE(iCount).
            iCount2 = iCount2 + 1.
            iCount = iCount + 1.
            antalDStabeller = antalDStabeller - 1.
         END.
         hdprotkopbertempbuffh = inSchaktProtDSh:GET-BUFFER-HANDLE(iCount).
         iCount = iCount + 1.
         hdschakprothandtempbuffh = inSchaktProtDSh:GET-BUFFER-HANDLE(iCount).
         iCount = iCount + 1.
         hdkalktempbuffh = inSchaktProtDSh:GET-BUFFER-HANDLE(iCount).
         iCount = iCount + 1.
         schkordstartsluttempbuffh = inSchaktProtDSh:GET-BUFFER-HANDLE(iCount).
         iCount = iCount + 1.
         sumkbuffh = inSchaktProtDSh:GET-BUFFER-HANDLE(iCount).
      END. 
      
      THIS-OBJECT:SchaktProtDSh = inSchaktProtDSh.
   END METHOD.
   METHOD PUBLIC VOID CreateCmbUtf():
      DEFINE VARIABLE cmbkataloglist AS Controls.GridRubrikLista NO-UNDO.
      DEFINE VARIABLE katanamnK      AS Controls.GridRubrik      NO-UNDO.
      cmbkataloglist = NEW Controls.GridRubrikLista().
      cmbkataloglist:ttBufferHandle = TEMP-TABLE uftfort:DEFAULT-BUFFER-HANDLE.
      katanamnK = NEW Controls.GridRubrik().
      katanamnK:Falt = "BENAMNING".
      katanamnK:Rubrik = "Visa".
      cmbkataloglist:addRubrik(katanamnK).
      CREATE uftfort.
      ASSIGN 
      uftfort.UTYP = 1
      uftfort.TYPC = "Alla".
      CREATE uftfort.
      ASSIGN 
      uftfort.UTYP = 2
      uftfort.TYPC = "Utförda".
      
      CREATE uftfort.
      ASSIGN 
      uftfort.UTYP = 3
      uftfort.TYPC = "Ej utförda".
      THIS-OBJECT:CmbUtf:GuruInitCombo(cmbkataloglist:ttBufferHandle, "UTYP", "TYPC").
      THIS-OBJECT:CmbUtf:VALUE = 1. 
   END METHOD.
   METHOD PUBLIC VOID CreateGridSchaktProt():
      {GridRubrikListaVarExtent.i}
      DEFINE VARIABLE CellFont AS System.Drawing.Font NO-UNDO.
      DEFINE VARIABLE irubrik AS INTEGER NO-UNDO.
      DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
      DEFINE VARIABLE iCount2 AS INTEGER NO-UNDO.
      DEFINE VARIABLE decCount AS INTEGER NO-UNDO.
      DEFINE VARIABLE iband AS INTEGER NO-UNDO.
      DEFINE VARIABLE hideBands AS INTEGER EXTENT 10 NO-UNDO.
      DEFINE VARIABLE sortBands AS INTEGER EXTENT 50 NO-UNDO.
      DEFINE VARIABLE sortCount AS INTEGER NO-UNDO INITIAL 1.
      DEFINE VARIABLE antalDStabeller AS INTEGER NO-UNDO.
      
      DEFINE VARIABLE queryvar          AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      
      
      antalDStabeller = THIS-OBJECT:antalProtTabeller.
      iCount = 1.
       
      THIS-OBJECT:HmtRubrikerlistaJoin = NEW Controls.GridRubrikListaJoin().
      iCount = 1.
      
      iCount = iCount + 1.
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iCount] = NEW Controls.GridRubrikListaJoin().
      
      DO WHILE antalDStabeller NE 0:
         THIS-OBJECT:HmtRubrikerlistaJoinTotal[iCount] = NEW Controls.GridRubrikListaJoin().
         iCount = iCount + 1.
         antalDStabeller = antalDStabeller - 1.
      END.
      iCount = iCount + 1.
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iCount] = NEW Controls.GridRubrikListaJoin().
      iCount = iCount + 1.
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iCount] = NEW Controls.GridRubrikListaJoin().
      iCount = iCount + 1.
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iCount] = NEW Controls.GridRubrikListaJoin().
      iCount = iCount + 1.
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iCount] = NEW Controls.GridRubrikListaJoin().
      iCount = iCount + 1.
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iCount] = NEW Controls.GridRubrikListaJoin().
      
      THIS-OBJECT:GridSchaktProt:GuruGridRadOption(TRUE).
      CellFont = NEW System.Drawing.Font("Arial Black", Progress.Util.CastUtil:ToSingle(15), System.Drawing.FontStyle:Bold, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).   
                                             /*styrfält , värde ,   färg ,                          font, alla col, lista col  0  bgcolor 1 = fg color 2 font*/
                                                                                                                          /*  ska avslutas med,*/ 
      THIS-OBJECT:GridSchaktProt:GuruAddRadOption("TYPSNITT", "1", Guru.StaticMethods:DotNetColor(12),CellFont,TRUE,"",2,0).
      
      /*start band*/        
      iband = 0.
      /*topbuffh*/
      rubrikergrid[1] = NEW Controls.GridRubrik("NAMN","Innehåll",TRUE,100).
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 1,INPUT THIS-OBJECT:topbuffh).
      iband = iband + 1.
      
      /*hdschakttempbuffh*/
      rubrikergrid[1] = NEW Controls.GridRubrik("BENAMNING","Schakt",TRUE,50).
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 1,INPUT THIS-OBJECT:hdschakttempbuffh).
      iband = iband + 1.
      
      /*hdschakprottempbuffh*/
      antalDStabeller = THIS-OBJECT:antalProtTabeller.   
      iCount2 = 1.
      DO WHILE antalDStabeller NE 0:
         irubrik = 1.
         rubrikergrid[irubrik] = NEW Controls.GridRubrik("UTFORT","Utfört",TRUE,100).
         irubrik = irubrik + 1.
         rubrikergrid[irubrik] = NEW Controls.GridRubrik("FRANTILL","Från - Till",TRUE,100).
         irubrik = irubrik + 1.
         IF Guru.GlobalaVariabler:SprofilAuto = 1 THEN DO:
            rubrikergrid[irubrik] = NEW Controls.GridRubrik("SPROFIL","SchaktProfil",TRUE,100).
            irubrik = irubrik + 1.
         END.   
         THIS-OBJECT:hdschakprottempbuffh[iCount2]:FIND-FIRST() NO-ERROR.
         IF THIS-OBJECT:hdschakprottempbuffh[iCount2]:AVAILABLE THEN DO:
            queryvar = "FOR EACH " + THIS-OBJECT:hdprotkopbertempbuffh:TABLE + " WHERE SID = " + THIS-OBJECT:hdschakprottempbuffh[iCount2]:BUFFER-FIELD("SID"):BUFFER-VALUE + " AND ORDNING < 49 BY ORDNING".
            CREATE QUERY qH.
            qH:ADD-BUFFER(hdprotkopbertempbuffh).
            qH:QUERY-PREPARE(queryvar).
            qH:QUERY-OPEN().    
            qH:GET-FIRST().
            DO WHILE qH:QUERY-OFF-END = FALSE:
               decCount = decCount + 1.
               rubrikergrid[irubrik] = NEW Controls.GridRubrik("DEC" + STRING(decCount),THIS-OBJECT:hdprotkopbertempbuffh:BUFFER-FIELD("RUBRIK"):BUFFER-VALUE + " (" + THIS-OBJECT:hdprotkopbertempbuffh:BUFFER-FIELD("ENHET"):BUFFER-VALUE + ")",TRUE, 100, "SCHAKTP").
               irubrik = irubrik + 1.
               qH:GET-NEXT().  
            END.
            antalDStabeller = antalDStabeller - 1.
            rubrikergrid[irubrik] = NEW Controls.GridRubrik("OVRIGT","Övrigt",TRUE,100).
            THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT irubrik, INPUT THIS-OBJECT:hdschakprottempbuffh[iCount2]).
            sortBands[sortCount] = iband.
            sortCount = sortCount + 1.
            iCount2 = iCount2 + 1.
            iband = iband + 1.
            decCount = 0.
         END.
         ELSE antalDStabeller = antalDStabeller - 1.   
      END.
             /*hdprotkopbertempbuffh*/
      rubrikergrid[1] = NEW Controls.GridRubrik("SID","SID",TRUE,100).
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 1,INPUT THIS-OBJECT:hdprotkopbertempbuffh).
      hideBands[1] = iband.
      iband = iband + 1.
      
      /*hdschakprothandtempbuffh*/
      rubrikergrid[1] = NEW Controls.GridRubrik("SID","SID",TRUE,100).
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 1,INPUT THIS-OBJECT:hdschakprothandtempbuffh).
      hideBands[2] = iband.
      iband = iband + 1.
      
      /*hdkalktempbuffh*/
      rubrikergrid[1] = NEW Controls.GridRubrik("ARBKOD","Kod",TRUE,30).
      rubrikergrid[2] = NEW Controls.GridRubrik("LOPNR","Löpnr",TRUE,30,"KALKLOP").
      rubrikergrid[3] = NEW Controls.GridRubrik("ANTAL","Antal",TRUE,30,"DEC","KALKYL").
      rubrikergrid[4] = NEW Controls.GridRubrik("ENHET","Enh",TRUE,30).
      rubrikergrid[5] = NEW Controls.GridRubrik("BENAMNING","Benämning",TRUE,300).
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 5,INPUT THIS-OBJECT:hdkalktempbuffh).
     
      iband = iband + 1.
      /*schkordstartsluttempbuffh*/
      rubrikergrid[1] = NEW Controls.GridRubrik("FRANTILL","Från - Till",TRUE,100).
      rubrikergrid[2] = NEW Controls.GridRubrik("XKORDF","Från X-kord",TRUE,100).
      rubrikergrid[3] = NEW Controls.GridRubrik("YKORDF","Från Y-kord",TRUE,100).
      rubrikergrid[4] = NEW Controls.GridRubrik("ZKORDF","Från Z-kord",TRUE,100).
      rubrikergrid[5] = NEW Controls.GridRubrik("XKORDT","Till X-kord",TRUE,100).
      rubrikergrid[6] = NEW Controls.GridRubrik("YKORDT","Till Y-kord",TRUE,100).
      rubrikergrid[7] = NEW Controls.GridRubrik("ZKORDT","Till Z-kord",TRUE,100).

      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 7,INPUT THIS-OBJECT:schkordstartsluttempbuffh).
      iband = iband + 1.
             
      /*sumkbuffh*/
      rubrikergrid[1] = NEW Controls.GridRubrik("RUBRIK","Rubrik",TRUE,150).
      rubrikergrid[2] = NEW Controls.GridRubrik("ANTAL","Antal",TRUE,100, "SCHAKTP").
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 2,INPUT THIS-OBJECT:sumkbuffh).
      iband = iband + 1.
      
      THIS-OBJECT:GridSchaktProt:GuruInitGridJoin(THIS-OBJECT:HmtRubrikerlistaJoinTotal,iband,DATASET-HANDLE SchaktProtDSh).
    
      

      /*THIS-OBJECT:GridSchaktProt:SetViewStyleBand(TRUE).*/
      THIS-OBJECT:GridSchaktProt:Text = "Schaktprotokoll". 
      
      
      THIS-OBJECT:GridSchaktProt:GuruHiddenBand(hideBands[1], TRUE).
      
      THIS-OBJECT:GridSchaktProt:GuruHiddenBand(hideBands[2], TRUE).



      IF Guru.GlobalaVariabler:RubrikSortSchakt = 1 THEN DO:
         REPEAT:
           THIS-OBJECT:GridSchaktProt:SortBandColl(sortBands[sortCount]).
           sortCount = sortCount - 1.
           IF sortCount < 1 THEN LEAVE.
         END. 
      END.
      
   END METHOD.
   

   METHOD PRIVATE VOID InitializeComponent ( ):
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
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Beredning.Schaktvisning").
      THIS-OBJECT:splitContainerMain = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerVal = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:CmbUtf = NEW Controls.ComboSimple().
      THIS-OBJECT:LabelKatalog = NEW Infragistics.Win.Misc.UltraLabel().
      THIS-OBJECT:gridSchaktProt = NEW Controls.GridJoin().
      THIS-OBJECT:ButtonOK = NEW Infragistics.Win.Misc.UltraButton().
      CAST(THIS-OBJECT:splitContainerMain, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerMain:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerMain:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerMain:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerVal, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerVal:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerVal:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerVal:SuspendLayout().
      CAST(THIS-OBJECT:CmbUtf, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:gridSchaktProt, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainerMain */
      /*  */
      THIS-OBJECT:splitContainerMain:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerMain:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerMain:Name = "splitContainerMain".
      THIS-OBJECT:splitContainerMain:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerMain.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerMain:Panel1:Controls:Add(THIS-OBJECT:splitContainerVal).
      /*  */
      /* splitContainerMain.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerMain:Panel2:Controls:Add(THIS-OBJECT:ButtonOK).
      THIS-OBJECT:splitContainerMain:Size = NEW System.Drawing.Size(1184, 862).
      THIS-OBJECT:splitContainerMain:SplitterDistance = 818.
      THIS-OBJECT:splitContainerMain:TabIndex = 0.
      /*  */
      /* splitContainerVal */
      /*  */
      THIS-OBJECT:splitContainerVal:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerVal:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerVal:Name = "splitContainerVal".
      THIS-OBJECT:splitContainerVal:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerVal.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerVal:Panel1:Controls:Add(THIS-OBJECT:CmbUtf).
      THIS-OBJECT:splitContainerVal:Panel1:Controls:Add(THIS-OBJECT:LabelKatalog).
      THIS-OBJECT:splitContainerVal:Panel1Collapsed = TRUE.
      /*  */
      /* splitContainerVal.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerVal:Panel2:Controls:Add(THIS-OBJECT:gridSchaktProt).
      THIS-OBJECT:splitContainerVal:Size = NEW System.Drawing.Size(1184, 818).
      THIS-OBJECT:splitContainerVal:SplitterDistance = 100.
      THIS-OBJECT:splitContainerVal:TabIndex = 2.
      /*  */
      /* CmbUtf */
      /*  */
      THIS-OBJECT:CmbUtf:DisplayStyle = Infragistics.Win.EmbeddableElementDisplayStyle:VisualStudio2005.
      THIS-OBJECT:CmbUtf:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
      THIS-OBJECT:CmbUtf:Location = NEW System.Drawing.Point(496, 13).
      THIS-OBJECT:CmbUtf:Name = "CmbUtf".
      THIS-OBJECT:CmbUtf:Size = NEW System.Drawing.Size(277, 21).
      THIS-OBJECT:CmbUtf:TabIndex = 44.
      THIS-OBJECT:CmbUtf:ValueChanged:Subscribe(THIS-OBJECT:CmbUtf_ValueChanged).
      /*  */
      /* LabelKatalog */
      /*  */
      appearance1:TextHAlignAsString = "Left".
      THIS-OBJECT:LabelKatalog:Appearance = appearance1.
      THIS-OBJECT:LabelKatalog:Location = NEW System.Drawing.Point(412, 14).
      THIS-OBJECT:LabelKatalog:Name = "LabelKatalog".
      THIS-OBJECT:LabelKatalog:Size = NEW System.Drawing.Size(65, 23).
      THIS-OBJECT:LabelKatalog:TabIndex = 43.
      THIS-OBJECT:LabelKatalog:Text = "Visa med".
      /*  */
      /* gridSchaktProt */
      /*  */
      appearance2:BackColor = System.Drawing.SystemColors:Window.
      appearance2:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridSchaktProt:DisplayLayout:Appearance = appearance2.
      THIS-OBJECT:gridSchaktProt:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridSchaktProt:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance3:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance3:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance3:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance3:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridSchaktProt:DisplayLayout:GroupByBox:Appearance = appearance3.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridSchaktProt:DisplayLayout:GroupByBox:BandLabelAppearance = appearance4.
      THIS-OBJECT:gridSchaktProt:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance5:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance5:BackColor2 = System.Drawing.SystemColors:Control.
      appearance5:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance5:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridSchaktProt:DisplayLayout:GroupByBox:PromptAppearance = appearance5.
      THIS-OBJECT:gridSchaktProt:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridSchaktProt:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridSchaktProt:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance6:BackColor = System.Drawing.SystemColors:Window.
      appearance6:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridSchaktProt:DisplayLayout:Override:ActiveCellAppearance = appearance6.
      appearance7:BackColor = System.Drawing.SystemColors:Highlight.
      appearance7:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridSchaktProt:DisplayLayout:Override:ActiveRowAppearance = appearance7.
      THIS-OBJECT:gridSchaktProt:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridSchaktProt:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance8:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridSchaktProt:DisplayLayout:Override:CardAreaAppearance = appearance8.
      appearance9:BorderColor = System.Drawing.Color:Silver.
      appearance9:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridSchaktProt:DisplayLayout:Override:CellAppearance = appearance9.
      THIS-OBJECT:gridSchaktProt:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridSchaktProt:DisplayLayout:Override:CellPadding = 0.
      appearance10:BackColor = System.Drawing.SystemColors:Control.
      appearance10:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance10:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance10:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance10:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridSchaktProt:DisplayLayout:Override:GroupByRowAppearance = appearance10.
      appearance11:TextHAlignAsString = "Left".
      THIS-OBJECT:gridSchaktProt:DisplayLayout:Override:HeaderAppearance = appearance11.
      THIS-OBJECT:gridSchaktProt:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridSchaktProt:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance12:BackColor = System.Drawing.SystemColors:Window.
      appearance12:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridSchaktProt:DisplayLayout:Override:RowAppearance = appearance12.
      THIS-OBJECT:gridSchaktProt:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance13:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridSchaktProt:DisplayLayout:Override:TemplateAddRowAppearance = appearance13.
      THIS-OBJECT:gridSchaktProt:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridSchaktProt:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridSchaktProt:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridSchaktProt:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridSchaktProt:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridSchaktProt:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridSchaktProt:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridSchaktProt:GuruContext = ?.
      THIS-OBJECT:gridSchaktProt:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridSchaktProt:Name = "gridSchaktProt".
      THIS-OBJECT:gridSchaktProt:Size = NEW System.Drawing.Size(1184, 818).
      THIS-OBJECT:gridSchaktProt:TabIndex = 1.
      THIS-OBJECT:gridSchaktProt:Text = "gridJoin1".
      THIS-OBJECT:gridSchaktProt:VisibleChanged:Subscribe(THIS-OBJECT:gridSchaktProt_VisibleChanged).
      /*  */
      /* ButtonOK */
      /*  */
      THIS-OBJECT:ButtonOK:Anchor = System.Windows.Forms.AnchorStyles:Right.
      appearance14:Image = CAST(resources:GetObject("appearance14.Image"), System.Object).
      THIS-OBJECT:ButtonOK:Appearance = appearance14.
      THIS-OBJECT:ButtonOK:Location = NEW System.Drawing.Point(1062, 5).
      THIS-OBJECT:ButtonOK:Name = "ButtonOK".
      THIS-OBJECT:ButtonOK:Size = NEW System.Drawing.Size(110, 28).
      THIS-OBJECT:ButtonOK:TabIndex = 3.
      THIS-OBJECT:ButtonOK:TabStop = FALSE.
      THIS-OBJECT:ButtonOK:Text = " OK".
      THIS-OBJECT:ButtonOK:Click:Subscribe(THIS-OBJECT:ButtonOK_Click).
      /*  */
      /* Schaktvisning */
      /*  */
      THIS-OBJECT:ClientSize = NEW System.Drawing.Size(1184, 862).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerMain).
      THIS-OBJECT:Name = "Schaktvisning".
      THIS-OBJECT:Text = "Schaktvisning".
      THIS-OBJECT:splitContainerMain:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerMain:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerMain, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerMain:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerVal:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerVal:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerVal:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerVal, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerVal:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:CmbUtf, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:gridSchaktProt, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
   END METHOD.



END CLASS.