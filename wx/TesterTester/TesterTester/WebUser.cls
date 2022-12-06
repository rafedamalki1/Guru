 /*------------------------------------------------------------------------
    File        : WebUser
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Fri Oct 28 15:56:22 CEST 2016
    Notes       : 
  ----------------------------------------------------------------------*/
USING Progress.Windows.Form.
  

CLASS Modules.Register.WebUser INHERITS Form   : 

	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
	DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlDataDb                 AS Modules.Register.AnvSparrDb                                      NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainer1 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE HmtRubrikerlista            AS Controls.GridRubrikLista                                        NO-UNDO.
   DEFINE PRIVATE VARIABLE ButtonOK AS Infragistics.Win.Misc.UltraButton NO-UNDO.
   DEFINE PRIVATE VARIABLE GridUserSparr AS Controls.Grid NO-UNDO.
   DEFINE {&PUBLIC} VARIABLE usersparrtth         AS HANDLE NO-UNDO.
    
	CONSTRUCTOR PUBLIC WebUser( ):
		SUPER().
		InitializeComponent ( ).
		CATCH e AS Progress.Lang.Error:
			UNDO, THROW e.
		END CATCH.		

	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC WebUser( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.
		
   METHOD PUBLIC VOID CreateGridUserSparr():
      
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      THIS-OBJECT:usersparrtth:FIND-FIRST("WHERE  ") NO-ERROR.
      IF THIS-OBJECT:usersparrtth:AVAILABLE THEN.
      ELSE DO:
         rrr = System.Windows.Forms.MessageBox:Show(Guru.konstanter:globalroot:LanguageManager:GetStringAsMessage(197), "").        
      END.
      DEFINE VARIABLE CellFont AS System.Drawing.Font NO-UNDO.
      /*DEFINE VARIABLE antallev AS INTEGER NO-UNDO.
      DEFINE VARIABLE antallevstart AS INTEGER NO-UNDO.
      DEFINE VARIABLE Prisvar AS CHARACTER NO-UNDO {GridRubrikListaExtent.i}.
      DEFINE VARIABLE Rubrikvar AS CHARACTER NO-UNDO {GridRubrikListaExtent.i}.*/
      {GridRubrikListaVarExtent.i}
      /*DEFINE VARIABLE rb AS CHARACTER NO-UNDO.*/
      rubrikergrid[1] = NEW Controls.GridRubrik("APPID","Ip/Användare",TRUE,180).
      rubrikergrid[2] = NEW Controls.GridRubrik("IDUSER","Ip/Användare",TRUE,310).
      rubrikergrid[3] = NEW Controls.GridRubrik("STDATE","Startdatum",TRUE,310).
      rubrikergrid[4] = NEW Controls.GridRubrik("EXDATE","Slutdatum",TRUE,310).
      rubrikergrid[5] = NEW Controls.GridRubrik("SOKCHAR[1]","Antal försök",TRUE,310).
      /*DEFINE VARIABLE CC AS CHARACTER NO-UNDO.
      antallevstart = 3.
      antallev = antallevstart.
      queryvar =  "FOR EACH " + THIS-OBJECT:rubrikTTh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:rubrikTTh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         antallev = antallev + 1.
         
         rubrikergrid[antallev] = NEW Controls.GridRubrik("PRIS",antallev - antallevstart,THIS-OBJECT:rubrikTTh:BUFFER-FIELD("LEVNAMN"):BUFFER-VALUE,TRUE,250,"DEC","").
                                                       
         qH:GET-NEXT().
      END.
      */
      THIS-OBJECT:GridUserSparr:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE, INPUT 5, INPUT THIS-OBJECT:usersparrtth)).
      THIS-OBJECT:GridUserSparr:SetTitle("Inloggningsförsök från ipadresser och users").       
      THIS-OBJECT:GridUserSparr:GuruRadOptionStart(TRUE).
      
      
      
      CellFont = NEW System.Drawing.Font("", Progress.Util.CastUtil:ToSingle(15), System.Drawing.FontStyle:Regular, System.Drawing.GraphicsUnit:Point, System.Convert:ToByte(0)).
         
                                                 /*styrfält , värde ,   färg ,                          font, alla col, lista col  0 = bgcolor 1 = fg color 2 font*/
                                                                                                                             /*  ska avslutas med,*/ 
      
      /*THIS-OBJECT:GridJmfPris:GuruRadOptionAdd("PRIS","MINPRIS","", Guru.StaticMethods:DotNetColor(10),CellFont,FALSE,"PRIS,",0,0).
      THIS-OBJECT:GridJmfPris:DisplayLayout:Bands[0]:Columns["MINPRIS"]:Hidden = TRUE.
      THIS-OBJECT:GridJmfPris:GuruReOpen().
      THIS-OBJECT:GridJmfPris:SetTitle("Materiel priser").*/
         
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
		
      THIS-OBJECT:splitContainer1 = NEW System.Windows.Forms.SplitContainer().
      CAST(THIS-OBJECT:splitContainer1, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainer1:SuspendLayout().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainer1 */
      /*  */
      THIS-OBJECT:splitContainer1:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainer1:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainer1:Name = "splitContainer1".
      THIS-OBJECT:splitContainer1:Size = NEW System.Drawing.Size(1194, 572).
      THIS-OBJECT:splitContainer1:SplitterDistance = 398.
      THIS-OBJECT:splitContainer1:TabIndex = 2.
      /*  */
      /* WebUser */
      /*  */
      THIS-OBJECT:ClientSize = NEW System.Drawing.Size(1194, 572).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainer1).
      THIS-OBJECT:FormBorderStyle = System.Windows.Forms.FormBorderStyle:FixedDialog.
      THIS-OBJECT:MaximizeBox = FALSE.
      THIS-OBJECT:MinimizeBox = FALSE.
      THIS-OBJECT:Name = "WebUser".
      THIS-OBJECT:ShowInTaskbar = FALSE.
      THIS-OBJECT:StartPosition = System.Windows.Forms.FormStartPosition:CenterParent.
      THIS-OBJECT:Text = "WebUser".
      CAST(THIS-OBJECT:splitContainer1, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainer1:ResumeLayout(FALSE).
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD. 
 
	METHOD PUBLIC VOID ShowModalDialog(  ):
		
		WAIT-FOR THIS-OBJECT:ShowDialog().

	END METHOD.


 
END CLASS.
