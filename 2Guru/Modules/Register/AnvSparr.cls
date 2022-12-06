 /*------------------------------------------------------------------------
    File        : AnvSparr
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Fri Oct 28 15:56:22 CEST 2016
    Notes       : 
  ----------------------------------------------------------------------*/
USING Progress.Windows.Form.
  

CLASS Modules.Register.AnvSparr INHERITS Form   : 

   DEFINE PRIVATE VARIABLE ButtonAvsluta AS Infragistics.Win.Misc.UltraButton NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonAvslutaL AS Infragistics.Win.Misc.UltraButton NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonHamta AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonLasUpp AS Controls.Button NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonSok AS Controls.Button NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE FranDatum AS Controls.FillinDate NO-UNDO.
   DEFINE PRIVATE VARIABLE gridLyck AS Controls.Grid NO-UNDO.
   DEFINE PRIVATE VARIABLE GridSparrU AS Controls.Grid NO-UNDO.
	DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   
   DEFINE PUBLIC VARIABLE AVSparrDataDb                 AS Modules.Register.AnvSparrDb                                      NO-UNDO.
 
   DEFINE PRIVATE VARIABLE HmtRubrikerlista            AS Controls.GridRubrikLista                                        NO-UNDO.
   
   DEFINE PRIVATE VARIABLE SokAnv AS Controls.FillinChar NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerSOK AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerMiss AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerLyck AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE TillDatum AS Controls.FillinDate NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PRIVATE VARIABLE TabManager AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabLycklogg AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabFellogg AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE VARIABLE skapadGridLyck AS LOGICAL NO-UNDO.
    
	CONSTRUCTOR PUBLIC AnvSparr( r AS Guru.Root ):
	
		SUPER().
		THIS-OBJECT:ROOT = r.
		InitializeComponent ( ).
		THIS-OBJECT:AVSparrDataDb = THIS-OBJECT:Root:DatabaseManager:AnvSparr.
		
		THIS-OBJECT:HmtRubrikerlista = NEW Controls.GridRubrikLista().

      THIS-OBJECT:AVSparrDataDb:InitializeTTs().

      THIS-OBJECT:CreateForm().
		CATCH e AS Progress.Lang.Error:
			UNDO, THROW e.
		END CATCH.		

	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC AnvSparr( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.
	
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID AnvSparr_Shown( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:GridSparrU:GuruAvmarkeraTot().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonAvslutaL_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:CLOSE().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonHamta_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		THIS-OBJECT:AVSparrDataDb:AnvSparrTracking(FALSE).
		THIS-OBJECT:AVSparrDataDb:AnvSparr().
		THIS-OBJECT:GridSparrU:GuruReopen().
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonLasUpp_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):	   
	   DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE rid AS ROWID                             NO-UNDO.
      THIS-OBJECT:GridSparrU:PerformAction(Infragistics.Win.UltraWinGrid.UltraGridAction:ExitEditMode)  NO-ERROR.
      IF THIS-OBJECT:GridSparrU:ActiveRow NE ? THEN DO:
         THIS-OBJECT:GridSparrU:SyncMan().
         IF THIS-OBJECT:GridSparrU:Posn() NE -1 THEN DO: 
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(208), THIS-OBJECT:Root:LanguageManager:GetString(44), System.Windows.Forms.MessageBoxButtons:YesNo, System.Windows.Forms.MessageBoxIcon:Question).
            IF rrr:ToString() NE "Yes" THEN RETURN.            
            THIS-OBJECT:GridSparrU:GuruBortrader(FALSE).
            
            AVSparrDataDb:Lasuppspara().
           
         END.
         ELSE DO:
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(76), THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(75)).
         END.
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
   METHOD PRIVATE VOID ButtonAvsluta_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):    
      THIS-OBJECT:CLOSE().
      RETURN.

   END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ButtonSok_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		DEFINE VARIABLE sokvar AS CHARACTER NO-UNDO.
		
      sokvar = "DATE(LOGGRAPP.DATUM) " + " >= " + STRING(THIS-OBJECT:FranDatum:GuruText:Datum).
      sokvar = sokvar + " AND  DATE(LOGGRAPP.DATUM) " + " <= " + STRING(THIS-OBJECT:TillDatum:GuruText:Datum).
      
      IF SokAnv:GuruText:Value = ? THEN.
      ELSE IF TRIM(STRING(SokAnv:GuruText:Value)) NE "" THEN DO: 
         sokvar = sokvar + " AND LOGGRAPP.OSANVANDARE = " + QUOTER(TRIM(STRING(SokAnv:GuruText:Value))).  
      END.   
      
      THIS-OBJECT:AVSparrDataDb:LoggHmt(sokvar).
      IF skapadGridLyck = FALSE THEN THIS-OBJECT:CreateGridLyck(). 
      THIS-OBJECT:GridLyck:GuruReopen().
		RETURN.

	END METHOD.

	METHOD PUBLIC VOID CreateForm():
	  
      THIS-OBJECT:Text = "Inloggninglista och spärrade användare ".
      THIS-OBJECT:SokAnv:RUBRIK = "OS-Användare".
      THIS-OBJECT:FranDatum:GuruLabel:TEXT = "Från".
      THIS-OBJECT:TillDatum:GuruLabel:TEXT = "Till".
      THIS-OBJECT:FranDatum:GuruText:Datum = TODAY.
      THIS-OBJECT:TillDatum:GuruText:Datum = TODAY.
     
      THIS-OBJECT:CreateGrids().
   END METHOD.
   METHOD PUBLIC VOID CreateGrids():
      THIS-OBJECT:CreateGridSparrU().
      THIS-OBJECT:CreateGridLyck().
   END METHOD.
		
   METHOD PUBLIC VOID CreateGridSparrU():
      DEFINE VARIABLE CellFont AS System.Drawing.Font NO-UNDO.  
      
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("SOKCHAR2","DatorAnvändare",TRUE,150,"CHAR","").
      rubrikergrid[2] = NEW Controls.GridRubrik("SOKCHAR5","GuruAnvändare",TRUE,150,"CHAR","").
      rubrikergrid[3] = NEW Controls.GridRubrik("APPID","DatorAnvändare/Ip/GuruAnvändare",TRUE,230).
      rubrikergrid[4] = NEW Controls.GridRubrik("IDUSER","Spärrad pga",TRUE,130).
      rubrikergrid[5] = NEW Controls.GridRubrik("SOKCHAR3","Från IP",TRUE,130,"CHAR","").
      rubrikergrid[6] = NEW Controls.GridRubrik("SOKCHAR",6,"Från PC",TRUE,130,"CHAR","").
      rubrikergrid[7] = NEW Controls.GridRubrik("SOKCHAR",1,"Antal försök",TRUE,130,"CHAR","").
      rubrikergrid[8] = NEW Controls.GridRubrik("SOKCHAR",4,"Senaste inloggning",TRUE,200,"CHAR","").
      rubrikergrid[9] = NEW Controls.GridRubrik("SOKCHAR",5,"Stopp!",TRUE,200,"CHAR","").
      THIS-OBJECT:GridSparrU:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid, INPUT FALSE, INPUT 9, INPUT THIS-OBJECT:AVSparrDataDb:usersparrbuffh)).
      THIS-OBJECT:GridSparrU:SetTitle("Inloggningsförsök från Ipadresser och Användare").       
      THIS-OBJECT:GridSparrU:GuruRadOptionStart(TRUE).
      THIS-OBJECT:GridSparrU:GuruOrderby("BY SOKCHAR2 BY SOKCHAR5").
      CellFont = NEW System.Drawing.Font("", Progress.Util.CastUtil:ToSingle(15), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
         
                                                 /*styrfält , värde ,   färg ,                          font, alla col, lista col  0 = bgcolor 1 = fg color 2 font*/
                                                                                                                    /*  lista col ska avslutas med,*/ 
     
      THIS-OBJECT:GridSparrU:GuruRadOptionAdd("","SOKCHAR[1]", STRING(Guru.Konstanter:varforetypval[52]), Guru.StaticMethods:DotNetColor(12),CellFont,TRUE,",",0,8).
  /*    THIS-OBJECT:GridSparrU:DisplayLayout:Bands[0]:Columns["IDUSER"]:Hidden = TRUE.*/
      THIS-OBJECT:GridSparrU:GuruReopen().
      
       
   END METHOD.  
   METHOD PUBLIC VOID CreateGridLyck():
      IF NOT VALID-HANDLE(THIS-OBJECT:AVSparrDataDb:userloggbuffh) THEN RETURN.
      skapadGridLyck = TRUE.
      DEFINE VARIABLE CellFont AS System.Drawing.Font NO-UNDO.  
      {GridRubrikListaVarExtent.i}
     
      rubrikergrid[1] = NEW Controls.GridRubrik("OSANVANDARE","Användare",TRUE,150,"CHAR","").
      rubrikergrid[2] = NEW Controls.GridRubrik("ANVANDARE","GuruAnvändare",TRUE,150,"CHAR","").
      rubrikergrid[3] = NEW Controls.GridRubrik("DATOR","Dator",TRUE,230).
      rubrikergrid[4] = NEW Controls.GridRubrik("DATUM","Tidpunkt",TRUE,130).
      rubrikergrid[5] = NEW Controls.GridRubrik("KLIENT","Progress version",TRUE,130,"CHAR","").
      rubrikergrid[6] = NEW Controls.GridRubrik("PID","Process ID",TRUE,130).
      rubrikergrid[7] = NEW Controls.GridRubrik("PROGRAM","Inloggad från",TRUE,200,"CHAR","").
      rubrikergrid[8] = NEW Controls.GridRubrik("URLSITE","Website",TRUE,200,"CHAR","").
      THIS-OBJECT:GridLyck:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid, INPUT FALSE, INPUT 8, INPUT THIS-OBJECT:AVSparrDataDb:userloggbuffh)).
      THIS-OBJECT:GridLyck:SetTitle("Inloggningar i Guru").       
       
      THIS-OBJECT:GridLyck:GuruOrderby("BY DATUM BY OSANVANDARE BY ANVANDARE").
      
      THIS-OBJECT:GridLyck:GuruReopen(). 
    
                 
   END METHOD.  
      

   METHOD PRIVATE VOID okButton_Click (sender AS System.Object, e AS System.EventArgs):
		THIS-OBJECT:DialogResult = System.Windows.Forms.DialogResult:Ok.
		THIS-OBJECT:Close ( ). 
	END METHOD.

	METHOD PRIVATE VOID cancelButton_Click (sender AS System.Object, e AS System.EventArgs):
		THIS-OBJECT:DialogResult = System.Windows.Forms.DialogResult:Cancel.
		THIS-OBJECT:Close ( ).
	END METHOD.
    
	METHOD PRIVATE VOID InitializeComponent ( ):

		/* NOTE: The following method is automatically generated.
		   
		   We strongly suggest that the contents of this method only be modified using the 
		   Visual Designer to avoid any incompatible modifications.
		   
		   Modifying the contents of this method using a code editor will invalidate any support for this file. */
		
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance1 AS Infragistics.Win.Appearance NO-UNDO.
      appearance1 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Register.AnvSparr").
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
      DEFINE VARIABLE ultraTab14 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab14 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance30 AS Infragistics.Win.Appearance NO-UNDO.
      appearance30 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance31 AS Infragistics.Win.Appearance NO-UNDO.
      appearance31 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab17 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab17 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance32 AS Infragistics.Win.Appearance NO-UNDO.
      appearance32 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance33 AS Infragistics.Win.Appearance NO-UNDO.
      appearance33 = NEW Infragistics.Win.Appearance().
      THIS-OBJECT:TabFellogg = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerMiss = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:ButtonHamta = NEW Controls.Button().
      THIS-OBJECT:ButtonAvsluta = NEW Infragistics.Win.Misc.UltraButton().
      THIS-OBJECT:ButtonLasUpp = NEW Controls.Button().
      THIS-OBJECT:GridSparrU = NEW Controls.Grid().
      THIS-OBJECT:TabLycklogg = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerLyck = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerSOK = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:TillDatum = NEW Controls.FillinDate().
      THIS-OBJECT:FranDatum = NEW Controls.FillinDate().
      THIS-OBJECT:ButtonSok = NEW Controls.Button().
      THIS-OBJECT:SokAnv = NEW Controls.FillinChar().
      THIS-OBJECT:ButtonAvslutaL = NEW Infragistics.Win.Misc.UltraButton().
      THIS-OBJECT:gridLyck = NEW Controls.Grid().
      THIS-OBJECT:TabManager = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:TabFellogg:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerMiss, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerMiss:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerMiss:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerMiss:SuspendLayout().
      CAST(THIS-OBJECT:GridSparrU, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabLycklogg:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerLyck, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerLyck:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerLyck:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerLyck:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerSOK, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerSOK:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerSOK:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerSOK:SuspendLayout().
      CAST(THIS-OBJECT:gridLyck, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:TabManager, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabManager:SuspendLayout().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* TabFellogg */
      /*  */
      THIS-OBJECT:TabFellogg:Controls:Add(THIS-OBJECT:splitContainerMiss).
      THIS-OBJECT:TabFellogg:Location = NEW System.Drawing.Point(1, 1).
      THIS-OBJECT:TabFellogg:Name = "TabFellogg".
      THIS-OBJECT:TabFellogg:Size = NEW System.Drawing.Size(1390, 522).
      /*  */
      /* splitContainerMiss */
      /*  */
      THIS-OBJECT:splitContainerMiss:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerMiss:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerMiss:Name = "splitContainerMiss".
      THIS-OBJECT:splitContainerMiss:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerMiss.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerMiss:Panel1:Controls:Add(THIS-OBJECT:ButtonHamta).
      THIS-OBJECT:splitContainerMiss:Panel1:Controls:Add(THIS-OBJECT:ButtonAvsluta).
      THIS-OBJECT:splitContainerMiss:Panel1:Controls:Add(THIS-OBJECT:ButtonLasUpp).
      /*  */
      /* splitContainerMiss.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerMiss:Panel2:Controls:Add(THIS-OBJECT:GridSparrU).
      THIS-OBJECT:splitContainerMiss:Size = NEW System.Drawing.Size(1390, 522).
      THIS-OBJECT:splitContainerMiss:SplitterDistance = 30.
      THIS-OBJECT:splitContainerMiss:TabIndex = 1.
      /*  */
      /* ButtonHamta */
      /*  */
      appearance1:Image = CAST(resources:GetObject("appearance1.Image"), System.Object).
      THIS-OBJECT:ButtonHamta:Appearance = appearance1.
      THIS-OBJECT:ButtonHamta:Font = NEW System.Drawing.Font("Microsoft Sans Serif", Progress.Util.CastUtil:ToSingle(6.75), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
      THIS-OBJECT:ButtonHamta:Location = NEW System.Drawing.Point(116, -1).
      THIS-OBJECT:ButtonHamta:Name = "ButtonHamta".
      THIS-OBJECT:ButtonHamta:Size = NEW System.Drawing.Size(110, 28).
      THIS-OBJECT:ButtonHamta:TabIndex = 5.
      THIS-OBJECT:ButtonHamta:Text = "Hämta data igen".
      THIS-OBJECT:ButtonHamta:Click:Subscribe(THIS-OBJECT:ButtonHamta_Click).
      /*  */
      /* ButtonAvsluta */
      /*  */
      DEFINE VARIABLE nestedvar0 AS System.Windows.Forms.AnchorStyles NO-UNDO.
      nestedvar0 = System.Windows.Forms.AnchorStyles:Top.
      THIS-OBJECT:ButtonAvsluta:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar0, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
      appearance2:Image = CAST(resources:GetObject("appearance2.Image"), System.Object).
      THIS-OBJECT:ButtonAvsluta:Appearance = appearance2.
      THIS-OBJECT:ButtonAvsluta:Location = NEW System.Drawing.Point(1280, 3).
      THIS-OBJECT:ButtonAvsluta:Name = "ButtonAvsluta".
      THIS-OBJECT:ButtonAvsluta:Size = NEW System.Drawing.Size(110, 28).
      THIS-OBJECT:ButtonAvsluta:TabIndex = 4.
      THIS-OBJECT:ButtonAvsluta:Text = " Avsluta".
      THIS-OBJECT:ButtonAvsluta:Click:Subscribe(THIS-OBJECT:ButtonAvsluta_Click).
      /*  */
      /* ButtonLasUpp */
      /*  */
      appearance3:Image = CAST(resources:GetObject("appearance3.Image"), System.Object).
      THIS-OBJECT:ButtonLasUpp:Appearance = appearance3.
      THIS-OBJECT:ButtonLasUpp:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:ButtonLasUpp:Name = "ButtonLasUpp".
      THIS-OBJECT:ButtonLasUpp:Size = NEW System.Drawing.Size(110, 28).
      THIS-OBJECT:ButtonLasUpp:TabIndex = 1.
      THIS-OBJECT:ButtonLasUpp:Text = "Lås upp".
      THIS-OBJECT:ButtonLasUpp:Click:Subscribe(THIS-OBJECT:ButtonLasUpp_Click).
      /*  */
      /* GridSparrU */
      /*  */
      appearance4:BackColor = System.Drawing.SystemColors:Window.
      appearance4:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridSparrU:DisplayLayout:Appearance = appearance4.
      THIS-OBJECT:GridSparrU:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridSparrU:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance5:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance5:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance5:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance5:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSparrU:DisplayLayout:GroupByBox:Appearance = appearance5.
      appearance6:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridSparrU:DisplayLayout:GroupByBox:BandLabelAppearance = appearance6.
      THIS-OBJECT:GridSparrU:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance7:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance7:BackColor2 = System.Drawing.SystemColors:Control.
      appearance7:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance7:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridSparrU:DisplayLayout:GroupByBox:PromptAppearance = appearance7.
      THIS-OBJECT:GridSparrU:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridSparrU:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridSparrU:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance8:BackColor = System.Drawing.SystemColors:Window.
      appearance8:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridSparrU:DisplayLayout:Override:ActiveCellAppearance = appearance8.
      appearance9:BackColor = System.Drawing.SystemColors:Highlight.
      appearance9:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridSparrU:DisplayLayout:Override:ActiveRowAppearance = appearance9.
      THIS-OBJECT:GridSparrU:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridSparrU:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance10:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSparrU:DisplayLayout:Override:CardAreaAppearance = appearance10.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      appearance11:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridSparrU:DisplayLayout:Override:CellAppearance = appearance11.
      THIS-OBJECT:GridSparrU:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridSparrU:DisplayLayout:Override:CellPadding = 0.
      appearance12:BackColor = System.Drawing.SystemColors:Control.
      appearance12:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance12:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance12:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance12:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridSparrU:DisplayLayout:Override:GroupByRowAppearance = appearance12.
      appearance13:TextHAlignAsString = "Left".
      THIS-OBJECT:GridSparrU:DisplayLayout:Override:HeaderAppearance = appearance13.
      THIS-OBJECT:GridSparrU:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridSparrU:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance14:BackColor = System.Drawing.SystemColors:Window.
      appearance14:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridSparrU:DisplayLayout:Override:RowAppearance = appearance14.
      THIS-OBJECT:GridSparrU:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance15:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridSparrU:DisplayLayout:Override:TemplateAddRowAppearance = appearance15.
      THIS-OBJECT:GridSparrU:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridSparrU:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridSparrU:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridSparrU:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridSparrU:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridSparrU:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridSparrU:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridSparrU:GuruContext = ?.
      THIS-OBJECT:GridSparrU:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridSparrU:Name = "GridSparrU".
      THIS-OBJECT:GridSparrU:Size = NEW System.Drawing.Size(1390, 488).
      THIS-OBJECT:GridSparrU:TabIndex = 0.
      THIS-OBJECT:GridSparrU:Text = "grid1".
      /*  */
      /* TabLycklogg */
      /*  */
      THIS-OBJECT:TabLycklogg:Controls:Add(THIS-OBJECT:splitContainerLyck).
      THIS-OBJECT:TabLycklogg:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabLycklogg:Name = "TabLycklogg".
      THIS-OBJECT:TabLycklogg:Size = NEW System.Drawing.Size(1390, 522).
      /*  */
      /* splitContainerLyck */
      /*  */
      THIS-OBJECT:splitContainerLyck:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerLyck:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerLyck:Name = "splitContainerLyck".
      THIS-OBJECT:splitContainerLyck:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerLyck.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerLyck:Panel1:Controls:Add(THIS-OBJECT:splitContainerSOK).
      /*  */
      /* splitContainerLyck.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerLyck:Panel2:Controls:Add(THIS-OBJECT:gridLyck).
      THIS-OBJECT:splitContainerLyck:Size = NEW System.Drawing.Size(1390, 522).
      THIS-OBJECT:splitContainerLyck:SplitterDistance = 114.
      THIS-OBJECT:splitContainerLyck:TabIndex = 0.
      /*  */
      /* splitContainerSOK */
      /*  */
      THIS-OBJECT:splitContainerSOK:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerSOK:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerSOK:Name = "splitContainerSOK".
      /*  */
      /* splitContainerSOK.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerSOK:Panel1:Controls:Add(THIS-OBJECT:TillDatum).
      THIS-OBJECT:splitContainerSOK:Panel1:Controls:Add(THIS-OBJECT:FranDatum).
      THIS-OBJECT:splitContainerSOK:Panel1:Controls:Add(THIS-OBJECT:ButtonSok).
      THIS-OBJECT:splitContainerSOK:Panel1:Controls:Add(THIS-OBJECT:SokAnv).
      /*  */
      /* splitContainerSOK.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerSOK:Panel2:Controls:Add(THIS-OBJECT:ButtonAvslutaL).
      THIS-OBJECT:splitContainerSOK:Size = NEW System.Drawing.Size(1390, 114).
      THIS-OBJECT:splitContainerSOK:SplitterDistance = 933.
      THIS-OBJECT:splitContainerSOK:TabIndex = 2.
      /*  */
      /* TillDatum */
      /*  */
      THIS-OBJECT:TillDatum:Location = NEW System.Drawing.Point(524, 11).
      THIS-OBJECT:TillDatum:Name = "TillDatum".
      THIS-OBJECT:TillDatum:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:TillDatum:TabIndex = 8.
      /*  */
      /* FranDatum */
      /*  */
      THIS-OBJECT:FranDatum:Location = NEW System.Drawing.Point(47, 11).
      THIS-OBJECT:FranDatum:Name = "FranDatum".
      THIS-OBJECT:FranDatum:Size = NEW System.Drawing.Size(282, 28).
      THIS-OBJECT:FranDatum:TabIndex = 7.
      /*  */
      /* ButtonSok */
      /*  */
      THIS-OBJECT:ButtonSok:Location = NEW System.Drawing.Point(689, 57).
      THIS-OBJECT:ButtonSok:Name = "ButtonSok".
      THIS-OBJECT:ButtonSok:Size = NEW System.Drawing.Size(75, 23).
      THIS-OBJECT:ButtonSok:TabIndex = 3.
      THIS-OBJECT:ButtonSok:Text = "Sök".
      THIS-OBJECT:ButtonSok:Click:Subscribe(THIS-OBJECT:ButtonSok_Click).
      /*  */
      /* SokAnv */
      /*  */
      THIS-OBJECT:SokAnv:Location = NEW System.Drawing.Point(47, 57).
      THIS-OBJECT:SokAnv:Name = "SokAnv".
      THIS-OBJECT:SokAnv:Rubrik = "GuruLabel".
      THIS-OBJECT:SokAnv:Size = NEW System.Drawing.Size(282, 33).
      THIS-OBJECT:SokAnv:TabIndex = 2.
      /*  */
      /* ButtonAvslutaL */
      /*  */
      DEFINE VARIABLE nestedvar1 AS System.Windows.Forms.AnchorStyles NO-UNDO.
      nestedvar1 = System.Windows.Forms.AnchorStyles:Top.
      THIS-OBJECT:ButtonAvslutaL:Anchor = CAST(Progress.Util.EnumHelper:Or(nestedvar1, System.Windows.Forms.AnchorStyles:Right), System.Windows.Forms.AnchorStyles).
      appearance16:Image = CAST(resources:GetObject("appearance16.Image"), System.Object).
      THIS-OBJECT:ButtonAvslutaL:Appearance = appearance16.
      THIS-OBJECT:ButtonAvslutaL:Location = NEW System.Drawing.Point(343, 3).
      THIS-OBJECT:ButtonAvslutaL:Name = "ButtonAvslutaL".
      THIS-OBJECT:ButtonAvslutaL:Size = NEW System.Drawing.Size(110, 28).
      THIS-OBJECT:ButtonAvslutaL:TabIndex = 5.
      THIS-OBJECT:ButtonAvslutaL:Text = " Avsluta".
      THIS-OBJECT:ButtonAvslutaL:Click:Subscribe(THIS-OBJECT:ButtonAvslutaL_Click).
      /*  */
      /* gridLyck */
      /*  */
      appearance17:BackColor = System.Drawing.SystemColors:Window.
      appearance17:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridLyck:DisplayLayout:Appearance = appearance17.
      THIS-OBJECT:gridLyck:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridLyck:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance18:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance18:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance18:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance18:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridLyck:DisplayLayout:GroupByBox:Appearance = appearance18.
      appearance19:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridLyck:DisplayLayout:GroupByBox:BandLabelAppearance = appearance19.
      THIS-OBJECT:gridLyck:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance20:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance20:BackColor2 = System.Drawing.SystemColors:Control.
      appearance20:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance20:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridLyck:DisplayLayout:GroupByBox:PromptAppearance = appearance20.
      THIS-OBJECT:gridLyck:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridLyck:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridLyck:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance21:BackColor = System.Drawing.SystemColors:Window.
      appearance21:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridLyck:DisplayLayout:Override:ActiveCellAppearance = appearance21.
      appearance22:BackColor = System.Drawing.SystemColors:Highlight.
      appearance22:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridLyck:DisplayLayout:Override:ActiveRowAppearance = appearance22.
      THIS-OBJECT:gridLyck:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridLyck:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance23:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridLyck:DisplayLayout:Override:CardAreaAppearance = appearance23.
      appearance24:BorderColor = System.Drawing.Color:Silver.
      appearance24:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridLyck:DisplayLayout:Override:CellAppearance = appearance24.
      THIS-OBJECT:gridLyck:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridLyck:DisplayLayout:Override:CellPadding = 0.
      appearance25:BackColor = System.Drawing.SystemColors:Control.
      appearance25:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance25:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance25:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance25:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridLyck:DisplayLayout:Override:GroupByRowAppearance = appearance25.
      appearance26:TextHAlignAsString = "Left".
      THIS-OBJECT:gridLyck:DisplayLayout:Override:HeaderAppearance = appearance26.
      THIS-OBJECT:gridLyck:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridLyck:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance27:BackColor = System.Drawing.SystemColors:Window.
      appearance27:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridLyck:DisplayLayout:Override:RowAppearance = appearance27.
      THIS-OBJECT:gridLyck:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance28:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridLyck:DisplayLayout:Override:TemplateAddRowAppearance = appearance28.
      THIS-OBJECT:gridLyck:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridLyck:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridLyck:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridLyck:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridLyck:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridLyck:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridLyck:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridLyck:GuruContext = ?.
      THIS-OBJECT:gridLyck:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridLyck:Name = "gridLyck".
      THIS-OBJECT:gridLyck:Size = NEW System.Drawing.Size(1390, 404).
      THIS-OBJECT:gridLyck:TabIndex = 0.
      THIS-OBJECT:gridLyck:Text = "grid1".
      /*  */
      /* TabManager */
      /*  */
      appearance29:BackColor = System.Drawing.SystemColors:Control.
      THIS-OBJECT:TabManager:Appearance = appearance29.
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabFellogg).
      THIS-OBJECT:TabManager:Controls:Add(THIS-OBJECT:TabLycklogg).
      THIS-OBJECT:TabManager:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:TabManager:ImageSize = NEW System.Drawing.Size(40, 40).
      THIS-OBJECT:TabManager:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:TabManager:Name = "TabManager".
      THIS-OBJECT:TabManager:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
      THIS-OBJECT:TabManager:Size = NEW System.Drawing.Size(1394, 572).
      THIS-OBJECT:TabManager:TabIndex = 45.
      THIS-OBJECT:TabManager:TabOrientation = Infragistics.Win.UltraWinTabs.TabOrientation:BottomLeft.
      appearance30:Image = CAST(resources:GetObject("appearance30.Image"), System.Object).
      ultraTab14:ActiveAppearance = appearance30.
      appearance31:Image = CAST(resources:GetObject("appearance31.Image"), System.Object).
      ultraTab14:Appearance = appearance31.
      ultraTab14:Key = "FelLogg".
      ultraTab14:TabPage = THIS-OBJECT:TabFellogg.
      ultraTab14:Text = "".
      ultraTab14:ToolTipText = "Felaktiga inloggningar".
      appearance32:Image = CAST(resources:GetObject("appearance32.Image"), System.Object).
      ultraTab17:ActiveAppearance = appearance32.
      appearance33:Image = CAST(resources:GetObject("appearance33.Image"), System.Object).
      ultraTab17:Appearance = appearance33.
      ultraTab17:Key = "LyckLogg".
      ultraTab17:TabPage = THIS-OBJECT:TabLycklogg.
      ultraTab17:Text = "".
      ultraTab17:ToolTipText = "lyckade inloggningar".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 2 NO-UNDO.
      arrayvar0[1] = ultraTab14.
      arrayvar0[2] = ultraTab17.
      THIS-OBJECT:TabManager:Tabs:AddRange(arrayvar0).
      THIS-OBJECT:TabManager:SelectedTabChanging:Subscribe(THIS-OBJECT:TabManager_SelectedTabChanging).
      THIS-OBJECT:TabManager:SelectedTabChanged:Subscribe(THIS-OBJECT:TabManager_SelectedTabChanged).
      /*  */
      /* ultraTabSharedControlsPage1 */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1".
      THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(1390, 522).
      /*  */
      /* AnvSparr */
      /*  */
      THIS-OBJECT:ClientSize = NEW System.Drawing.Size(1394, 572).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:TabManager).
      THIS-OBJECT:FormBorderStyle = System.Windows.Forms.FormBorderStyle:FixedDialog.
      THIS-OBJECT:MaximizeBox = FALSE.
      THIS-OBJECT:MinimizeBox = FALSE.
      THIS-OBJECT:Name = "AnvSparr".
      THIS-OBJECT:ShowInTaskbar = FALSE.
      THIS-OBJECT:StartPosition = System.Windows.Forms.FormStartPosition:CenterParent.
      THIS-OBJECT:Text = "WebUser".
      THIS-OBJECT:Shown:Subscribe(THIS-OBJECT:AnvSparr_Shown).
      THIS-OBJECT:TabFellogg:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerMiss:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerMiss:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerMiss, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerMiss:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridSparrU, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabLycklogg:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerLyck:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerLyck:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerLyck, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerLyck:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSOK:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSOK:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerSOK, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerSOK:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:gridLyck, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:TabManager, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabManager:ResumeLayout(FALSE).
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD. 
 
	METHOD PUBLIC VOID ShowModalDialog(  ):
		
		WAIT-FOR THIS-OBJECT:ShowDialog().

	END METHOD.

	
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID TabManager_SelectedTabChanged( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangedEventArgs ):
      THIS-OBJECT:Invalidate().
      THIS-OBJECT:TabManager:Invalidate().
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE).
		RETURN.

	END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID TabManager_SelectedTabChanging( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinTabControl.SelectedTabChangingEventArgs ):
		DEFINE VARIABLE path  AS CHARACTER NO-UNDO.  
      DEFINE VARIABLE cTo   AS CHARACTER NO-UNDO.
      DEFINE VARIABLE cFrom AS CHARACTER NO-UNDO.
      cTo = e:TAB:KEY.
      cFrom = THIS-OBJECT:TabManager:SelectedTab:KEY NO-ERROR.
      
      
      IF cFrom EQ ? THEN cFrom = "".  
      IF cTo = "Fellogg" THEN DO:
          
         RETURN. 
      END.
      IF cTo = "LyckLogg" THEN DO:
         THIS-OBJECT:ButtonSok_Click( INPUT ?, INPUT ?).
         RETURN. 
      END.
            
      PROCESS EVENTS.
		

	END METHOD.
 
END CLASS.
