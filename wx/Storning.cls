 
 /*------------------------------------------------------------------------
    File        : Storning
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Thu Mar 23 10:20:03 CET 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.
USING Progress.Windows.Form.



CLASS Modules.Storning.Storning INHERITS UserControl: 

   DEFINE PRIVATE VARIABLE gridStorImp AS Controls.GridJoin NO-UNDO.
   DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   
   
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC  VARIABLE ModuleHandle                 AS Guru.Module                                                    NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlDataDb                 AS Modules.Storning.StorningDb                                      NO-UNDO.
   
   
   DEFINE PRIVATE VARIABLE HmtRubrikerlista             AS Controls.GridRubrikLista                                        NO-UNDO.
   DEFINE PUBLIC VARIABLE HmtRubrikerlistaJoin AS Controls.GridRubrikListaJoin NO-UNDO.
   {GridRubrikListaJoinVarExtent.i}
   DEFINE PRIVATE VARIABLE splitContainerImportMenyS AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerImportMain AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE VARIABLE StorningImportDSh AS HANDLE NO-UNDO.
   DEFINE PUBLIC VARIABLE toolStripBladdra AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE toolStripImport AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE toolStripLasIn AS System.Windows.Forms.ToolStripButton NO-UNDO.
   
   {StorImpDSFilinfofix.i}
      
   CONSTRUCTOR PUBLIC Storning (INPUT r AS Guru.Root):
      SUPER().
      THIS-OBJECT:ROOT = r.
      InitializeComponent ( ).
     
      THIS-OBJECT:Root:DatabaseManager:Storning:ControllStorning = THIS-OBJECT.
      
      THIS-OBJECT:ControlDataDb = THIS-OBJECT:Root:DatabaseManager:Storning.
      THIS-OBJECT:ControlDataDb:InitializeTTs().
     
      
    
      THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

   END CONSTRUCTOR.
  
   METHOD PUBLIC VOID CreateForm():
      THIS-OBJECT:CreateGrids().
      THIS-OBJECT:CreateRibbon().
      
     
   END METHOD.
   
   METHOD PUBLIC VOID CreateGrids():
      THIS-OBJECT:CreateGridStorImp(). 
   END METHOD.
   
   METHOD PUBLIC VOID CreateRibbon():
      
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
      rubrikergrid[1] = NEW Controls.GridRubrik("IdentifikationsnrForetag","IdentifikationsnrForetag",TRUE,72).
      rubrikergrid[2] = NEW Controls.GridRubrik("Foretag","Företag",TRUE,72).
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 2,INPUT TEMP-TABLE ForetagData:HANDLE:DEFAULT-BUFFER-HANDLE).
      iband = iband + 1.
      
      /*DistriktData*/  
      rubrikergrid[1] = NEW Controls.GridRubrik("IdentifikationsnrForetag","IdentifikationsnrForetag",TRUE,72).
      rubrikergrid[2] = NEW Controls.GridRubrik("DistriktsId","DistriktsId",TRUE,72).
      rubrikergrid[3] = NEW Controls.GridRubrik("Artal","Artal",TRUE,72).
      rubrikergrid[4] = NEW Controls.GridRubrik("DistriktsNamn","DistriktsNamn",TRUE,72).
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 4,INPUT TEMP-TABLE DistriktData:HANDLE:DEFAULT-BUFFER-HANDLE).
      iband = iband + 1.
      
      /*KundData*/
      rubrikergrid[1] = NEW Controls.GridRubrik("IdentifikationsnrForetag","IdentifikationsnrForetag",TRUE,72).
      rubrikergrid[2] = NEW Controls.GridRubrik("DistriktsId","DistriktsId",TRUE,72).
      rubrikergrid[3] = NEW Controls.GridRubrik("Artal","Artal",TRUE,72).
      rubrikergrid[4] = NEW Controls.GridRubrik("SpanNivId","SpanNivId",TRUE,72).
      rubrikergrid[5] = NEW Controls.GridRubrik("AntalKunderLuftnat","AntalKunderLuftnat",TRUE,72).
      rubrikergrid[6] = NEW Controls.GridRubrik("AntalKunderBlandNat","AntalKunderBlandNat",TRUE,72).
      rubrikergrid[7] = NEW Controls.GridRubrik("AntalKunderKabelNat","AntalKunderKabelNat",TRUE,72).
      rubrikergrid[8] = NEW Controls.GridRubrik("AntalKunderTotalt","AntalKunderTotalt",TRUE,72).
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 8,INPUT TEMP-TABLE KundData:HANDLE:DEFAULT-BUFFER-HANDLE).
      iband = iband + 1.
      
      /*LedningData*/
      rubrikergrid[1] = NEW Controls.GridRubrik("IdentifikationsnrForetag","IdentifikationsnrForetag",TRUE,72).
      rubrikergrid[2] = NEW Controls.GridRubrik("DistriktsId","DistriktsId",TRUE,72).
      rubrikergrid[3] = NEW Controls.GridRubrik("Artal","Artal",TRUE,72).
      rubrikergrid[4] = NEW Controls.GridRubrik("SpanNivId","SpanNivId",TRUE,72).
      rubrikergrid[5] = NEW Controls.GridRubrik("Text1","Faltbeskrivning1",TRUE,72).
      rubrikergrid[6] = NEW Controls.GridRubrik("Text2","Faltbeskrivning2",TRUE,72).
      rubrikergrid[7] = NEW Controls.GridRubrik("AntalLuftnat","AntalLuftnat",TRUE,72).
      rubrikergrid[8] = NEW Controls.GridRubrik("AntalBlandNat","AntalBlandNat",TRUE,72).
      rubrikergrid[9] = NEW Controls.GridRubrik("AntalKabelNat","AntalKabelNat",TRUE,72).
      rubrikergrid[10] = NEW Controls.GridRubrik("AntalTotalt","AntalTotalt",TRUE,72).
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 10,INPUT TEMP-TABLE LedningData:HANDLE:DEFAULT-BUFFER-HANDLE).
      iband = iband + 1.
      
      /*StorningData*/
      rubrikergrid[1] = NEW Controls.GridRubrik("IdentifikationsnrForetag","IdentifikationsnrForetag",TRUE,72).
      rubrikergrid[2] = NEW Controls.GridRubrik("DistriktsId","DistriktsId",TRUE,72).
      rubrikergrid[3] = NEW Controls.GridRubrik("StorId","StorId",TRUE,72).
      rubrikergrid[4] = NEW Controls.GridRubrik("StortypId","StortypId",TRUE,72).
      rubrikergrid[5] = NEW Controls.GridRubrik("StorningsDatum","StorningsDatum",TRUE,72).
      rubrikergrid[6] = NEW Controls.GridRubrik("StorningKlockan","StorningKlockan",TRUE,72).
      rubrikergrid[7] = NEW Controls.GridRubrik("SlutDatum70","SlutDatum70",TRUE,72).
      rubrikergrid[8] = NEW Controls.GridRubrik("SlutKlockan70","SlutKlockan70",TRUE,72).
      rubrikergrid[9] = NEW Controls.GridRubrik("SlutDatum","SlutDatum",TRUE,72).
      rubrikergrid[10] = NEW Controls.GridRubrik("SlutKlockan","SlutKlockan",TRUE,72).
      rubrikergrid[11] = NEW Controls.GridRubrik("KundAvbrottsTid","KundAvbrottsTid",TRUE,72).
      rubrikergrid[12] = NEW Controls.GridRubrik("AntalHSP","AntalHSP",TRUE,72).
      rubrikergrid[13] = NEW Controls.GridRubrik("AntalLSP","AntalLSP",TRUE,72).
      rubrikergrid[14] = NEW Controls.GridRubrik("AntalRegStn","AntalRegStn",TRUE,72).
      rubrikergrid[15] = NEW Controls.GridRubrik("AntalNatStn","AntalNatStn",TRUE,72).
      rubrikergrid[16] = NEW Controls.GridRubrik("BortKW","BortKW",TRUE,72).
      rubrikergrid[17] = NEW Controls.GridRubrik("FranspanId","FranspanId",TRUE,72).
      rubrikergrid[18] = NEW Controls.GridRubrik("FelspanId","FelspanId",TRUE,72).
      rubrikergrid[19] = NEW Controls.GridRubrik("BrytoId","BrytoId",TRUE,72).
      rubrikergrid[20] = NEW Controls.GridRubrik("BrytorgLit","BrytorgLit",TRUE,72).
      rubrikergrid[21] = NEW Controls.GridRubrik("SektionerId","SektionerId",TRUE,72).
      rubrikergrid[22] = NEW Controls.GridRubrik("SektionLit","SektionLit",TRUE,72).
      rubrikergrid[23] = NEW Controls.GridRubrik("UtlosId","UtlosId",TRUE,72).
      rubrikergrid[24] = NEW Controls.GridRubrik("RelinId","RelinId",TRUE,72).
      rubrikergrid[25] = NEW Controls.GridRubrik("FelYId","FelYId",TRUE,72).
      rubrikergrid[26] = NEW Controls.GridRubrik("StDriftId","StDriftId",TRUE,72).
      rubrikergrid[27] = NEW Controls.GridRubrik("StDriftLit","StDriftLit",TRUE,72).
      rubrikergrid[28] = NEW Controls.GridRubrik("NatStrukHSPID","NatStrukHSPID",TRUE,72).
      rubrikergrid[29] = NEW Controls.GridRubrik("NatStrukLSPID","NatStrukLSPID",TRUE,72).
      rubrikergrid[30] = NEW Controls.GridRubrik("NattypId","NattypId",TRUE,72).
      rubrikergrid[31] = NEW Controls.GridRubrik("AdelId","AdelId",TRUE,72).
      rubrikergrid[32] = NEW Controls.GridRubrik("FelOId","FelOId",TRUE,72).
      
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 32,INPUT TEMP-TABLE StorningData:HANDLE:DEFAULT-BUFFER-HANDLE).
      iband = iband + 1.
      
      /*StorFelMtrlData*/
      rubrikergrid[1] = NEW Controls.GridRubrik("IdentifikationsnrForetag","IdentifikationsnrFöretag",TRUE,72).
      rubrikergrid[2] = NEW Controls.GridRubrik("DistriktsId","DistriktsId",TRUE,72).
      rubrikergrid[3] = NEW Controls.GridRubrik("StorId","StorId",TRUE,72).
      rubrikergrid[4] = NEW Controls.GridRubrik("SfmId","SfmId",TRUE,72).
      rubrikergrid[5] = NEW Controls.GridRubrik("Apparat","Apparat",TRUE,72).
      rubrikergrid[6] = NEW Controls.GridRubrik("Leverantor","Leverantor",TRUE,72).
      rubrikergrid[7] = NEW Controls.GridRubrik("Fabrikat","Fabrikat",TRUE,72).
      rubrikergrid[8] = NEW Controls.GridRubrik("Artikelnr","Artikelnr",TRUE,72).
      rubrikergrid[9] = NEW Controls.GridRubrik("Artal","Artal",TRUE,72).
      rubrikergrid[10] = NEW Controls.GridRubrik("Antal","Antal",TRUE,72).
      THIS-OBJECT:HmtRubrikerlistaJoinTotal[iband + 1] = THIS-OBJECT:HmtRubrikerlistaJoin:CreateRubrik(INPUT iband,INPUT rubrikergrid,INPUT TRUE,INPUT 10,INPUT TEMP-TABLE StorFelMtrlData:HANDLE:DEFAULT-BUFFER-HANDLE).
      iband = iband + 1.
      
      StorningImportDSh = DATASET StorningImportDS:HANDLE.

      THIS-OBJECT:GridStorImp:GuruInitGridJoin(THIS-OBJECT:HmtRubrikerlistaJoinTotal,iband,DATASET-HANDLE StorningImportDSh).
      THIS-OBJECT:GridStorImp:SetViewStyleBand(TRUE).
      THIS-OBJECT:GridStorImp:Text = "Dra din fil hit för att importera (XML eller JSON)".
   END METHOD.
   
      /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridStorImp_DragDrop( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.DragEventArgs ):
      DEFINE VARIABLE res     AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE objFiles AS "System.String[]" NO-UNDO.  
      DEFINE VARIABLE felmed AS LOGICAL NO-UNDO.
      DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.    
      
      DEFINE VARIABLE theStream AS System.IO.MemoryStream NO-UNDO.
      DEFINE VARIABLE fileGroupDescriptor AS "System.Byte[]" NO-UNDO. 
      DEFINE VARIABLE filNamn AS System.Text.StringBuilder NO-UNDO.
      DEFINE VARIABLE intbyte AS INTEGER NO-UNDO.
      DEFINE VARIABLE i AS INTEGER NO-UNDO.
      DEFINE VARIABLE langd AS INTEGER NO-UNDO.
      
      
            MESSAGE "VAs"
      VIEW-AS ALERT-BOX.
      /* GENERELL DRAG AND DROP KOD*/                                                                                                                                                                                                                                        
      IF e:Data:GetDataPresent(System.Windows.Forms.DataFormats:FileDrop) EQ TRUE THEN DO:
         objFiles = CAST(e:Data:GetData(System.Windows.Forms.DataFormats:FileDrop),"System.String[]").
         cFile = objFiles:GetValue(0):ToString().
      END. 
      ELSE IF e:Data:GetDataPresent("FileGroupDescriptor") EQ TRUE THEN DO:
         i = 0.
         theStream = CAST(e:Data:GetData("FileGroupDescriptor"),"System.IO.MemoryStream").
         fileGroupDescriptor = NEW "System.Byte[]" (512).   
         langd = theStream:Length - 1.

         DO WHILE i < langd:
             intbyte = theStream:ReadByte().
             fileGroupDescriptor:SetValue(intbyte AS UNSIGNED-BYTE, i).
             i = i + 1.
         END.                 

         i = 76.
         filNamn = NEW System.Text.StringBuilder().

         DO WHILE i < langd:
            filNamn:Append(System.Convert:ToChar(fileGroupDescriptor:GetValue(i))).
            i = i + 1.
         END. 
         cFile = System.IO.Path:GetTempPath() + filNamn:ToString().
      END.
      
      /* INLÄSNINGSKOD*/
      IF cFile NE "" THEN DO:
         THIS-OBJECT:ImporteraFil(cFile).
      END.
                                                                                                                                                                                                                                                  
      RETURN.
      

   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID GridStorImp_DragEnter( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.DragEventArgs ):
      MESSAGE "VA"
      VIEW-AS ALERT-BOX.
      /*Drag and drop*/
      IF e:Data:GetDataPresent(System.Windows.Forms.DataFormats:FileDrop) EQ TRUE THEN DO:
        e:Effect = System.Windows.Forms.DragDropEffects:COPY.
      END.
      ELSE IF e:Data:GetDataPresent("FileGroupDescriptor") EQ TRUE THEN DO:
         e:Effect = System.Windows.Forms.DragDropEffects:COPY.
      END.
      ELSE DO:
         e:Effect = System.Windows.Forms.DragDropEffects:NONE.   
      END.
      RETURN.

   END METHOD.
   
   METHOD PUBLIC LOGICAL ImporteraFil(path AS CHARACTER):
      
      IF CAPS(SUBSTRING(path, LENGTH(path) - 3)) = ".XML" THEN DO:
         DATASET StorningImportDs:READ-XML("FILE", path, "EMPTY", ?,False).
      END.
      ELSE IF CAPS(SUBSTRING(path, LENGTH(path) - 4)) = ".JSON" THEN DO:
         MESSAGE "e"
         VIEW-AS ALERT-BOX.
         DATASET StorningImportDs:READ-JSON("FILE", path, "EMPTY").
         THIS-OBJECT:SkrivXml().
         /*BUGG FIX KOD*/
         /*
         FIND FIRST ForetagData.
         FIND FIRST DistriktsData.
         ASSIGN 
         DistriktsData.IdentifikationsnrForetag = ForetagData.IdentifikationsnrForetag
         .
         FOR EACH KundData:
            ASSIGN
               KundData.IdentifikationsnrForetag = KundData.IdentifikationsnrForetag
               KundData.DiskriktsId = KundData.IdentifikationsnrForetag
               KundData.Artal = KundData.IdentifikationsnrForetag.
            END.
            MESSAGE KundData.SpanNivId KundData.IdentifikationsnrForetag
            VIEW-AS ALERT-BOX.
         END.*/
         /*BUGG FIX KOD*/
      END.
      ELSE DO:
         MESSAGE "Fel filformat"
         VIEW-AS ALERT-BOX.  
         RETURN FALSE.
      END.
      
      THIS-OBJECT:SkapaExportTTREC().
      THIS-OBJECT:GridStorImp:Text = "Importerad fil: " + path.
      THIS-OBJECT:GridStorImp:GridData(DATASET-HANDLE StorningImportDSh).
      RETURN TRUE.
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
   
   METHOD PRIVATE VOID SkapaExportTTREC():
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

   METHOD PUBLIC LOGICAL Initialize():
      
      THIS-OBJECT:CreateForm().
      
      RETURN TRUE.
   END METHOD.

   METHOD PRIVATE VOID InitializeComponent ( ):
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Storning.Storning").
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
      THIS-OBJECT:splitContainerImportMain = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerImportMenyS = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:toolStripImport = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:toolStripBladdra = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:toolStripLasIn = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:gridStorImp = NEW Controls.GridJoin().
      CAST(THIS-OBJECT:splitContainerImportMain, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerImportMain:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerImportMain:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerImportMenyS, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerImportMenyS:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerImportMenyS:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerImportMenyS:SuspendLayout().
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
      THIS-OBJECT:splitContainerImportMain:Panel1:Controls:Add(THIS-OBJECT:splitContainerImportMenyS).
      THIS-OBJECT:splitContainerImportMain:Panel2Collapsed = TRUE.
      THIS-OBJECT:splitContainerImportMain:Size = NEW System.Drawing.Size(1184, 562).
      THIS-OBJECT:splitContainerImportMain:SplitterDistance = 820.
      THIS-OBJECT:splitContainerImportMain:TabIndex = 0.
      /*  */
      /* splitContainerImportMenyS */
      /*  */
      THIS-OBJECT:splitContainerImportMenyS:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerImportMenyS:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerImportMenyS:Name = "splitContainerImportMenyS".
      THIS-OBJECT:splitContainerImportMenyS:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerImportMenyS.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerImportMenyS:Panel1:Controls:Add(THIS-OBJECT:toolStripImport).
      /*  */
      /* splitContainerImportMenyS.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerImportMenyS:Panel2:Controls:Add(THIS-OBJECT:gridStorImp).
      THIS-OBJECT:splitContainerImportMenyS:Size = NEW System.Drawing.Size(1184, 562).
      THIS-OBJECT:splitContainerImportMenyS:SplitterDistance = 25.
      THIS-OBJECT:splitContainerImportMenyS:TabIndex = 1.
      /*  */
      /* toolStripImport */
      /*  */
      THIS-OBJECT:toolStripImport:Dock = System.Windows.Forms.DockStyle:Fill.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:toolStripBladdra.
      arrayvar0[2] = THIS-OBJECT:toolStripLasIn.
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
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridStorImp:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:gridStorImp:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridStorImp:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridStorImp:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridStorImp:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:gridStorImp:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridStorImp:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:gridStorImp:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridStorImp:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridStorImp:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridStorImp:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridStorImp:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:gridStorImp:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridStorImp:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridStorImp:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridStorImp:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:gridStorImp:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridStorImp:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridStorImp:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:gridStorImp:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:gridStorImp:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridStorImp:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridStorImp:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:gridStorImp:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridStorImp:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:gridStorImp:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridStorImp:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridStorImp:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridStorImp:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridStorImp:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridStorImp:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridStorImp:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridStorImp:GuruContext = ?.
      THIS-OBJECT:gridStorImp:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridStorImp:Name = "gridStorImp".
      THIS-OBJECT:gridStorImp:Size = NEW System.Drawing.Size(1184, 533).
      THIS-OBJECT:gridStorImp:TabIndex = 0.
      THIS-OBJECT:gridStorImp:Text = "grid1".
      THIS-OBJECT:gridStorImp:DragDrop:Subscribe(THIS-OBJECT:gridStorImp_DragDrop).
      THIS-OBJECT:gridStorImp:DragEnter:Subscribe(THIS-OBJECT:gridStorImp_DragEnter).
      /*  */
      /* Storning */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerImportMain).
      THIS-OBJECT:Name = "Storning".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1184, 562).
      THIS-OBJECT:splitContainerImportMain:Panel1:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerImportMain, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerImportMain:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerImportMenyS:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerImportMenyS:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerImportMenyS:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerImportMenyS, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerImportMenyS:ResumeLayout(FALSE).
      THIS-OBJECT:toolStripImport:ResumeLayout(FALSE).
      THIS-OBJECT:toolStripImport:PerformLayout().
      CAST(THIS-OBJECT:gridStorImp, System.ComponentModel.ISupportInitialize):EndInit().
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
	METHOD PRIVATE VOID toolStripBladdra_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE fildir  AS CHARACTER                         NO-UNDO.
      DEFINE VARIABLE globanv AS CHARACTER                         NO-UNDO.
      DEFINE VARIABLE OKvald  AS LOGICAL                           NO-UNDO.

      Guru.Konstanter:globanv = Guru.Konstanter:globanv.
      fildir = SESSION:TEMP-DIRECTORY.
      {SESSIONTEMPDIR.I}
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
      OS-CREATE-DIR VALUE(fildir) NO-ERROR.
      SYSTEM-DIALOG GET-FILE fildir
         TITLE          "Välj fil som ska läsas in"
         FILTERS        "All Files (*.xml;*.json)"  "*.xml;*.json"   
         INITIAL-DIR    fildir
         UPDATE OKvald.      
      IF OKvald = FALSE THEN RETURN.  
      
      THIS-OBJECT:ImporteraFil(fildir).
      
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID toolStripLasIn_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		
		RETURN.

	END METHOD.

END CLASS.