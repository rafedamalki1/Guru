 
 /*------------------------------------------------------------------------
    File        : StorningImpCon
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Mon Jun 12 13:50:50 CEST 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.



CLASS Modules.Storning.StorningImpCon INHERITS UserControl: 
   
   DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerImportMenySI AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE toolStripKontroll AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE toolStripLasIn AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE toolStripImport AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE toolStripBladdra AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerImportMain AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC  VARIABLE gridStorImp AS Controls.GridJoin NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC  VARIABLE ModuleHandle                 AS Guru.Module                                                    NO-UNDO.
   DEFINE PUBLIC  VARIABLE ControlDataDb                 AS Modules.Storning.StorningDb                                      NO-UNDO.
   DEFINE PUBLIC VARIABLE StorningImportDSh AS HANDLE NO-UNDO.
   {StorImpDSFilinfofix.i}   
   {GridRubrikListaJoinVarExtent.i} 
   DEFINE PUBLIC VARIABLE HmtRubrikerlistaJoin AS Controls.GridRubrikListaJoin NO-UNDO.
   CONSTRUCTOR PUBLIC StorningImpCon (INPUT r AS Guru.Root  ):
      
      
      SUPER().
      THIS-OBJECT:ROOT = r.
      InitializeComponent().
     
      THIS-OBJECT:ControlDataDb = THIS-OBJECT:Root:DatabaseManager:Storning.
     
      THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

   END CONSTRUCTOR.
   
   
   METHOD PRIVATE VOID InitializeComponent(  ):
      
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Storning.StorningImpCon").
      THIS-OBJECT:splitContainerImportMain = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerImportMenySI = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:toolStripImport = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:toolStripBladdra = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:toolStripKontroll = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:toolStripLasIn = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:gridStorImp = NEW Controls.GridJoin().
      CAST(THIS-OBJECT:splitContainerImportMain, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerImportMain:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerImportMain:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerImportMenySI, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerImportMenySI:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerImportMenySI:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerImportMenySI:SuspendLayout().
      THIS-OBJECT:toolStripImport:SuspendLayout().
      CAST(THIS-OBJECT:gridStorImp, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainerImportMain */
      /*  */
      THIS-OBJECT:splitContainerImportMain:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerImportMain:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerImportMain:Name = "splitContainerImportMain".
      /*  */
      /* splitContainerImportMain.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerImportMain:Panel1:Controls:Add(THIS-OBJECT:splitContainerImportMenySI).
      THIS-OBJECT:splitContainerImportMain:Panel2Collapsed = TRUE.
      THIS-OBJECT:splitContainerImportMain:Size = NEW System.Drawing.Size(1184, 562).
      THIS-OBJECT:splitContainerImportMain:SplitterDistance = 820.
      THIS-OBJECT:splitContainerImportMain:TabIndex = 1.
      /*  */
      /* splitContainerImportMenySI */
      /*  */
      THIS-OBJECT:splitContainerImportMenySI:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerImportMenySI:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerImportMenySI:Name = "splitContainerImportMenySI".
      THIS-OBJECT:splitContainerImportMenySI:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerImportMenySI.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerImportMenySI:Panel1:Controls:Add(THIS-OBJECT:toolStripImport).
      /*  */
      /* splitContainerImportMenySI.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerImportMenySI:Panel2:Controls:Add(THIS-OBJECT:gridStorImp).
      THIS-OBJECT:splitContainerImportMenySI:Size = NEW System.Drawing.Size(1184, 562).
      THIS-OBJECT:splitContainerImportMenySI:SplitterDistance = 25.
      THIS-OBJECT:splitContainerImportMenySI:TabIndex = 1.
      /*  */
      /* toolStripImport */
      /*  */
      THIS-OBJECT:toolStripImport:Dock = System.Windows.Forms.DockStyle:Fill.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 3 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:toolStripBladdra.
      arrayvar0[2] = THIS-OBJECT:toolStripKontroll.
      arrayvar0[3] = THIS-OBJECT:toolStripLasIn.
      THIS-OBJECT:toolStripImport:Items:AddRange(arrayvar0).
      THIS-OBJECT:toolStripImport:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:toolStripImport:Name = "toolStripImport".
      THIS-OBJECT:toolStripImport:Size = NEW System.Drawing.Size(1184, 25).
      THIS-OBJECT:toolStripImport:TabIndex = 1.
      THIS-OBJECT:toolStripImport:Text = "toolStrip1".
      /*  */
      /* toolStripBladdra */
      /*  */
      THIS-OBJECT:toolStripBladdra:Image = CAST(resources:GetObject("toolStripBladdra.Image"), System.Drawing.Image).
      THIS-OBJECT:toolStripBladdra:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:toolStripBladdra:Name = "toolStripBladdra".
      THIS-OBJECT:toolStripBladdra:Size = NEW System.Drawing.Size(67, 22).
      THIS-OBJECT:toolStripBladdra:Text = "Bläddra".
      THIS-OBJECT:toolStripBladdra:Click:Subscribe(THIS-OBJECT:toolStripBladdra_Click).
      /*  */
      /* toolStripKontroll */
      /*  */
      THIS-OBJECT:toolStripKontroll:Image = CAST(resources:GetObject("toolStripKontroll.Image"), System.Drawing.Image).
      THIS-OBJECT:toolStripKontroll:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:toolStripKontroll:Name = "toolStripKontroll".
      THIS-OBJECT:toolStripKontroll:Size = NEW System.Drawing.Size(97, 22).
      THIS-OBJECT:toolStripKontroll:Text = "Kontroll av fil".
      THIS-OBJECT:toolStripKontroll:Click:Subscribe(THIS-OBJECT:toolStripKontroll_Click).
      /*  */
      /* toolStripLasIn */
      /*  */
      THIS-OBJECT:toolStripLasIn:Image = CAST(resources:GetObject("toolStripLasIn.Image"), System.Drawing.Image).
      THIS-OBJECT:toolStripLasIn:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:toolStripLasIn:Name = "toolStripLasIn".
      THIS-OBJECT:toolStripLasIn:Size = NEW System.Drawing.Size(57, 22).
      THIS-OBJECT:toolStripLasIn:Text = "Läs in".
      THIS-OBJECT:toolStripLasIn:Click:Subscribe(THIS-OBJECT:toolStripLasIn_Click).
      /*  */
      /* gridStorImp */
      /*  */
      THIS-OBJECT:gridStorImp:AllowDrop = TRUE.
      THIS-OBJECT:gridStorImp:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridStorImp:GuruContext = ?.
      THIS-OBJECT:gridStorImp:GuruDroppable = TRUE.
      THIS-OBJECT:gridStorImp:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridStorImp:Name = "gridStorImp".
      THIS-OBJECT:gridStorImp:Size = NEW System.Drawing.Size(1184, 533).
      THIS-OBJECT:gridStorImp:TabIndex = 0.
      THIS-OBJECT:gridStorImp:Text = "Imp".
      /*  */
      /* StorningImpCon */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerImportMain).
      THIS-OBJECT:Name = "StorningImpCon".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1184, 562).
      THIS-OBJECT:splitContainerImportMain:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerImportMain, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerImportMain:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerImportMenySI:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerImportMenySI:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerImportMenySI:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerImportMenySI, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerImportMenySI:ResumeLayout(FALSE).
      THIS-OBJECT:toolStripImport:ResumeLayout(FALSE).
      THIS-OBJECT:toolStripImport:PerformLayout().
      CAST(THIS-OBJECT:gridStorImp, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridStorImp():
      DEFINE VARIABLE CellFont AS System.Drawing.Font NO-UNDO.
      {GridRubrikListaVarExtent.i}
      DEFINE VARIABLE irubrik AS INTEGER NO-UNDO.
      DEFINE VARIABLE iband AS INTEGER NO-UNDO.
      DEFINE VARIABLE queryvar          AS CHARACTER NO-UNDO.
      DEFINE VARIABLE qH                AS HANDLE    NO-UNDO.
      
      
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
     
       
      /*ForetagData*/ 
      irubrik = 1.  
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("IdentifikationsnrForetag","IdentifikationsnrForetag",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("Foretag","Företag",TRUE,72).       
     
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT irubrik,INPUT THIS-OBJECT:ControlDataDb:ForetagDBbuffh).
      iband = iband + 1.
      irubrik = 1.  
      /*DistriktData*/  
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("IdentifikationsnrForetag","IdentifikationsnrForetag",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("DistriktsId","DistriktsId",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("Artal","Artal",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("DistriktsNamn","DistriktsNamn",TRUE,72).
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT irubrik,INPUT THIS-OBJECT:ControlDataDb:DistriktDBbuffh).
      iband = iband + 1.
      irubrik = 1.  
      /*KundData*/
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("IdentifikationsnrForetag","IdentifikationsnrForetag",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("DistriktsId","DistriktsId",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("Artal","Artal",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("SpanNivId","SpanNivId",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("AntalKunderTotalt","AntalKunderTotalt",TRUE,72).
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT irubrik,THIS-OBJECT:ControlDataDb:KundDataDBbuffh).
      iband = iband + 1.
      irubrik = 1.  
      /*LedningData*/
     
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("IdentifikationsnrForetag","IdentifikationsnrForetag",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("DistriktsId","DistriktsId",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("Artal","Artal",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("SpanNivId","SpanNivId",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("Text1","Faltbeskrivning1",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("Text2","Faltbeskrivning2",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("AntalTotalt","AntalTotalt",TRUE,72).
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT irubrik,INPUT THIS-OBJECT:ControlDataDb:LedninggsDataDBbuffh).
      iband = iband + 1.
      irubrik = 1.  
      /*StorningData*/
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("IdentifikationsnrForetag","IdentifikationsnrForetag",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("DistriktsId","DistriktsId",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("StorId","StorId",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("StortypId","StortypId",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("StorningsDatum","StorningsDatum",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("StorningKlockan","StorningKlockan",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("SlutDatum","SlutDatum",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("SlutKlockan","SlutKlockan",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("KundAvbrottsTid","KundAvbrottsTid",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("AntalHSP","AntalHSP",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("AntalLSP","AntalLSP",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("BortKW","BortKW",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("FranspanId","FranspanId",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("FelspanId","FelspanId",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("BrytoId","BrytoId",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("BrytorgLit","BrytorgLit",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("SektionerId","SektionerId",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("SektionLit","SektionLit",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("UtlosId","UtlosId",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("RelinId","RelinId",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("FelYId","FelYId",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("StDriftId","StDriftId",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("StDriftLit","StDriftLit",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("AdelId","AdelId",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("FelOId","FelOId",TRUE,72).
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT irubrik,INPUT THIS-OBJECT:ControlDataDb:StorningsDataDBbuffh).
      iband = iband + 1.
      irubrik = 1.  
      /*StorFelMtrlData*/
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("IdentifikationsnrForetag","IdentifikationsnrFöretag",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("DistriktsId","DistriktsId",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("StorId","StorId",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("SfmId","SfmId",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("Apparat","Apparat",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("Leverantor","Leverantor",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("Fabrikat","Fabrikat",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("Artikelnr","Artikelnr",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("Artal","Artal",TRUE,72).
      irubrik = irubrik + 1.
      rubrikergrid[irubrik] = NEW Controls.GridRubrik("Antal","Antal",TRUE,72).
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT irubrik,INPUT THIS-OBJECT:ControlDataDb:StorFelMtrlDBbuffh).
      iband = iband + 1.
      irubrik = 1.  
      StorningImportDSh = DATASET StorningImportDS:HANDLE.

      THIS-OBJECT:GridStorImp:GuruInitGridJoin(THIS-OBJECT:HmtRubrikerlistaJoinTotal,iband,DATASET-HANDLE StorningImportDSh).
      THIS-OBJECT:GridStorImp:SetViewStyleBand(TRUE).
      THIS-OBJECT:GridStorImp:Text = "Dra din fil hit för att importera (XML eller JSON)".
      
      THIS-OBJECT:GridStorImp:DragDrop:Subscribe(THIS-OBJECT:GridStorImp:Grid_DragDropFile).
      THIS-OBJECT:GridStorImp:DragEnter:Subscribe(THIS-OBJECT:GridStorImp:Grid_DragEnter).
      THIS-OBJECT:GridStorImp:GuruAfterDropped:Subscribe(THIS-OBJECT:GridStorImp_GuruAfterDropped).
       /*Anders Olsson Elpool i Umeå AB  10 aug 2017 10:48:19 
   Bort enligt Matz Tapper
      rubrikergrid[7] = NEW Controls.GridRubrik("AntalLuftnat","AntalLuftnat",TRUE,72).
      rubrikergrid[8] = NEW Controls.GridRubrik("AntalBlandNat","AntalBlandNat",TRUE,72).
      rubrikergrid[9] = NEW Controls.GridRubrik("AntalKabelNat","AntalKabelNat",TRUE,72).
      */
       /*Anders Olsson Elpool i Umeå AB  10 aug 2017 10:48:19 
      Bort enligt Matz Tapper
      rubrikergrid[5] = NEW Controls.GridRubrik("AntalKunderLuftnat","AntalKunderLuftnat",TRUE,72).
      rubrikergrid[6] = NEW Controls.GridRubrik("AntalKunderBlandNat","AntalKunderBlandNat",TRUE,72).
      rubrikergrid[7] = NEW Controls.GridRubrik("AntalKunderKabelNat","AntalKunderKabelNat",TRUE,72).
      */
      /*Anders Olsson Elpool i Umeå AB  10 aug 2017 10:48:19 
        Bort enligt Matz Tapper 
      rubrikergrid[7] = NEW Controls.GridRubrik("SlutDatum70","SlutDatum70",TRUE,72).
      rubrikergrid[8] = NEW Controls.GridRubrik("SlutKlockan70","SlutKlockan70",TRUE,72).
      */
      
       /*Anders Olsson Elpool i Umeå AB  10 aug 2017 10:48:19 
   Bort enligt Matz Tapper
      rubrikergrid[14] = NEW Controls.GridRubrik("AntalRegStn","AntalRegStn",TRUE,72).
      rubrikergrid[15] = NEW Controls.GridRubrik("AntalNatStn","AntalNatStn",TRUE,72).
      */
      
      /*Anders Olsson Elpool i Umeå AB  10 aug 2017 10:46:04 
      Bort enligt Matz Tapper 
      rubrikergrid[28] = NEW Controls.GridRubrik("NatStrukHSPID","NatStrukHSPID",TRUE,72).
      rubrikergrid[29] = NEW Controls.GridRubrik("NatStrukLSPID","NatStrukLSPID",TRUE,72).
      rubrikergrid[28] = NEW Controls.GridRubrik("NattypId","NattypId",TRUE,72).
      */
       
   END METHOD.
   METHOD PUBLIC VOID GridStorImp_GuruAfterDropped(INPUT sender AS System.Object, INPUT e AS System.EventArgs):
     
      IF THIS-OBJECT:GridStorImp:DropedFile NE "" THEN DO:
         
         IF THIS-OBJECT:ControlDataDb:ImporteraFil(THIS-OBJECT:GridStorImp:DropedFile) = TRUE THEN  THIS-OBJECT:ControlDataDb:KontrollData().
      END.
   END METHOD.
   
	
     /*WRITE RÄTT*/
   METHOD PRIVATE VOID SkrivXML():
      /*
      DEFINE VARIABLE cTargetType     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE cFile           AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lFormatted      AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE cEncoding       AS CHARACTER NO-UNDO.
      DEFINE VARIABLE cSchemaLocation AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lWriteSchema    AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE lMinSchema      AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE lRetOK          AS LOGICAL   NO-UNDO.
      
      ASSIGN 
      cTargetType    =  "FILE"
      cFile           = "C:\PROTEMP10\sch1.xml"
      lFormatted      = TRUE
      cEncoding       = "UTF-8"
      lWriteSchema = TRUE 
      lMinSchema = TRUE.
      cFile           = "C:\PROTEMP10\sch1.json".
      DATASET StorningImportDs:WRITE-XML("FILE", "C:\PROTEMP10\" + "xmlexempel" + ".xml", lFormatted, cEncoding, cSchemaLocation, false, false).
      */
   END.
   METHOD PUBLIC VOID SkapaExportTTREC():
      DEFINE VARIABLE iBuff AS INTEGER NO-UNDO.
      DEFINE VARIABLE kommandosortquery AS CHARACTER NO-UNDO.
      DEFINE VARIABLE dynbuffh AS HANDLE NO-UNDO.
      DEFINE VARIABLE dynok AS LOGICAL NO-UNDO.
      DEFINE VARIABLE dynqueh AS HANDLE NO-UNDO.
      DEFINE VARIABLE dynfalth AS HANDLE NO-UNDO.
      DO iBuff = 1 TO DATASET StorningImportDs:NUM-BUFFERS:
         dynbuffh = DATASET StorningImportDs:GET-BUFFER-HANDLE(iBuff).
         CREATE QUERY dynqueh.
         dynqueh:SET-BUFFERS(dynbuffh).
         kommandosortquery = "FOR EACH " + dynbuffh:TABLE + ".".    
         dynok = dynqueh:QUERY-PREPARE(kommandosortquery).   
         dynok = dynqueh:QUERY-OPEN() NO-ERROR.
         REPEAT:
            dynqueh:GET-NEXT(NO-LOCK).
            IF dynqueh:QUERY-OFF-END THEN LEAVE.            
            dynfalth = dynbuffh:BUFFER-FIELD("TTRECID").
            dynfalth:BUFFER-VALUE = dynbuffh:RECID.                               
         END.     
      END.
      dynqueh:QUERY-CLOSE()  NO-ERROR. 
      DELETE OBJECT dynqueh NO-ERROR.
      dynqueh = ?.
      DELETE OBJECT dynbuffh NO-ERROR.
      dynbuffh = ?.
   
   END METHOD.
   
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID toolStripBladdra_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      DEFINE VARIABLE fildir  AS CHARACTER                         NO-UNDO.
      DEFINE VARIABLE OKvald  AS LOGICAL                           NO-UNDO.
      fildir = SESSION:TEMP-DIRECTORY.
      {SESSIONTEMPDIR.I}
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
      OS-CREATE-DIR VALUE(fildir) NO-ERROR.
      SYSTEM-DIALOG GET-FILE fildir
         TITLE          "Välj fil som ska läsas in"
         FILTERS        "All Files (*.xml;*.json,*.xlsx)"  "*.xml;*.json;*.xlsx"   
         INITIAL-DIR    fildir
         UPDATE OKvald.      
      IF OKvald = FALSE THEN RETURN.  
      IF THIS-OBJECT:ControlDataDb:ImporteraFil(fildir) = TRUE THEN  THIS-OBJECT:ControlDataDb:KontrollData().
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID toolStripKontroll_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlDataDb:KontrollData().
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID toolStripLasIn_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlDataDb:LasInData().
      RETURN.

   END METHOD.

   DESTRUCTOR PUBLIC StorningImpCon ( ):

   END DESTRUCTOR.

END CLASS.