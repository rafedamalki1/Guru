 
 /*------------------------------------------------------------------------
    File        : AllaDatabaser
    Purpose     : 
    Syntax      : run Modules\Register\AllaDatabaserStart.p.
    Description : 
    Author(s)   : elpao
    Created     : Mon Jun 13 11:28:27 CEST 2022
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Register.AllaDatabaser INHERITS Form: 
	{VALDBTEMP.I}
   DEFINE PRIVATE VARIABLE ANV AS Controls.FillinChar NO-UNDO.
   DEFINE PRIVATE VARIABLE PWD AS Controls.FillinChar NO-UNDO.
   DEFINE VARIABLE kolllosen  AS CHARACTER NO-UNDO CASE-SENSITIVE.
   DEFINE PUBLIC VARIABLE ServersEdit AS Infragistics.Win.UltraWinEditors.UltraTextEditor NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerAdb AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerStartLogin AS System.Windows.Forms.SplitContainer NO-UNDO.
	DEFINE VARIABLE ValdbTTh AS HANDLE NO-UNDO.
	DEFINE VARIABLE ValdbbuffTTh AS HANDLE NO-UNDO.
	DEFINE PRIVATE VARIABLE HmtRubrikerlista            AS Controls.GridRubrikLista                                        NO-UNDO.
	DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE GridAllaDb AS Controls.Grid NO-UNDO.
   
		
	CONSTRUCTOR PUBLIC AllaDatabaser (r AS Guru.Root  ):
		
		
      SUPER().
      THIS-OBJECT:ROOT = r.
      
      THIS-OBJECT:HmtRubrikerlista = NEW Controls.GridRubrikLista().
      InitializeComponent().
      THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
      THIS-OBJECT:CreateForm().
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

	END CONSTRUCTOR.
	CONSTRUCTOR PUBLIC AllaDatabaser (   ):
      
      
      SUPER().
       
      
      THIS-OBJECT:HmtRubrikerlista = NEW Controls.GridRubrikLista().
      InitializeComponent().
      THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
      THIS-OBJECT:CreateForm().
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

   END CONSTRUCTOR.
	METHOD PUBLIC VOID CreateForm():
	   {VALDBALL.I}
	   CREATE TEMP-TABLE ValdbTTh.
	   THIS-OBJECT:splitContainerStartLogin:Panel2Collapsed = TRUE.
      ValdbTTh:CREATE-LIKE("valdbtemp").
      ValdbTTh:ADD-NEW-FIELD("TTRECID","RECID").
      ValdbTTh:TEMP-TABLE-PREPARE("valdbtemp").
      THIS-OBJECT:ValdbbuffTTh = ValdbTTh:DEFAULT-BUFFER-HANDLE.
      FOR EACH valdbtemp:
         THIS-OBJECT:ValdbbuffTTh:BUFFER-CREATE().
         THIS-OBJECT:ValdbbuffTTh:BUFFER-COPY(BUFFER valdbtemp:HANDLE).
         THIS-OBJECT:ValdbbuffTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = THIS-OBJECT:ValdbbuffTTh:RECID.  
      END.   
      THIS-OBJECT:EditServers().  
      THIS-OBJECT:CreateGrids().
      THIS-OBJECT:ANV:GuruLabel:Text = "Användare".
      THIS-OBJECT:PWD:GuruLabel:Text = "Lösenord".
      THIS-OBJECT:PWD:GuruText:PasswordChar = "*".
       
   END METHOD.
   
     
   
   METHOD PUBLIC VOID EditServers():
      THIS-OBJECT:ServersEdit:TEXT = 
      "
      Server 1 " + CHR(10) + "
      Elpool-SRV01 " + CHR(49) + CHR(57) + CHR(50) + CHR(46) + CHR(49) + CHR(54) + CHR(56) + CHR(46) + CHR(55) + CHR(46) + CHR(50) + CHR(49)  + "  ->  " + {forsapp.I} + ":3328  infra    " + {forsapp.I} + " " + CHR(10) + "
      SFTP-server för Vattenfall Service Nordic
      Databas server Endast för Infratek " + CHR(10) + "
      Ska bytas till en Saas licens från Progress i oktober och byta till PASOE + kryptering " + CHR(10) + " 
      IP " + {forsapp.I} + CHR(10) + "
      -AppService APPFORS -H " + {forsapp.I} + " -S 2555 " + CHR(10) + " 
      TCP 2554  " + CHR(10) + "
      TCP 2600-2630  " + CHR(10) + "
      UDP 2555  " + CHR(10) + "
         
      Server 2 " + CHR(10) + "
      Elpool-SRV02 " + CHR(49) + CHR(57) + CHR(50) + CHR(46) + CHR(49) + CHR(54) + CHR(56) + CHR(46) + CHR(55) + CHR(46) + CHR(50) + CHR(50)  + "  ->   " + {forsapp.I} + ":3329  www2      " + {www2app.I} + " " + CHR(10) + "
      IP  " + {www2app.I} + " " + CHR(10) + "
      -AppService APPASP -H  " + {www2app.I} + " -S 2802' " + CHR(10) + " 
      TCP 2801   " + CHR(10) + "
      TCP2900-2989  " + CHR(10) + "
      UDP 2802      " + CHR(10) + "
      
      -AppService APPWWW2 -H " + {PasOewww2.i} + " -S 2671 -ssl' " + CHR(10) + "
      TCP 2672  Appwww2 " + CHR(10) + "
      TCP 2400-2500 " + CHR(10) + "
      UDP 2671  Namewww2  " + CHR(10) + " 
      
      Pasoe Saas kunder" + CHR(10) + 
      {PasOewww2.i}  + CHR(10) + "
      TCP 8445 " + CHR(10) + " 
      Server 3 " + CHR(10) + "
     
      Elpool-SRV03 " + CHR(49) + CHR(57) + CHR(50) + CHR(46) + CHR(49) + CHR(54) + CHR(56) + CHR(46) + CHR(55) + CHR(46) + CHR(50) + CHR(51)  + "   ->       " + {forsapp.I} + ":3330  egna     " + {egnaapp.I} + " " + CHR(10) + "
      IP " + {egnaapp.I} + " " + CHR(10) + "
      -URL https://egna.guruonweb.se:8445/apsv " + CHR(10) + "
      Pasoe            " + CHR(10) + " 
      TCP 8445         " + CHR(10) + "
      Endast Energiföretagen  " + CHR(10) + "
      för db darplu sekg " + CHR(10) +  
      {PasoeEgna.i} + CHR(10) + "   
      TCP 2672  AppEgen " + CHR(10) + "
      TCP 2400-2500 " + CHR(10) + "
      UDP 2671  NameEgen  " + CHR(10) + "
      -AppService APPSINGEL -H " + {egnaapp.I} + " -S 2665  " + CHR(10) + "
      Singel  " + CHR(10) + "
      TCP 2664 " + CHR(10) + "
      TCP 2631-2660 " + CHR(10) + "
      UDP 2665  " + CHR(10) + "
      Elkrafberedning " + CHR(10) + "
      -AppService APPELKB -H " + {egnaapp.I} + " -S 2555 " + CHR(10) + "
      TCP 2554  " + CHR(10) + "
      TCP 2600-2607 " + CHR(10) + "
      UDP 2555 " + CHR(10) + "
      -AppService APPSKOGSK -H " + {egnaapp.I} + " -S 2655 " + CHR(10) + "
      SkogsK " + CHR(10) + "
      TCP 2654 " + CHR(10) + "
      TCP 2608-2615 " + CHR(10) + "
      UDP 2655 " + CHR(10).


      
   END METHOD.
   METHOD PUBLIC VOID CreateGrids():
      
      THIS-OBJECT:CreateGridAllaDb().
      
      RETURN.
   END METHOD.
   METHOD PUBLIC VOID CreateGridAllaDb():
      {GridRubrikListaVarExtent.i}
    
      rubrikergrid[1] = NEW Controls.GridRubrik("FORETAG","Företag",TRUE,55).      
      rubrikergrid[2] = NEW Controls.GridRubrik("GFORETAG","Gföretag måste vara unikt per databas",TRUE,200).
      rubrikergrid[3] = NEW Controls.GridRubrik("VALDB","Företags Rubrik Visas vid val av databas",TRUE,200).
      rubrikergrid[4] = NEW Controls.GridRubrik("DBNAMN","Databas namn",TRUE,75).
      rubrikergrid[5] = NEW Controls.GridRubrik("DBPLATS","Databas mapp",TRUE,200).
      rubrikergrid[6] = NEW Controls.GridRubrik("DBCON","Databas anslutning",TRUE,255).
      rubrikergrid[7] = NEW Controls.GridRubrik("APPCON","Appserver anslutning",TRUE,355).
      rubrikergrid[8] = NEW Controls.GridRubrik("WWWSTART","Hämt ställe av Guruversioner",TRUE,355).
      rubrikergrid[9] = NEW Controls.GridRubrik("DBCACHE","Stoppa db",TRUE,65).
      /*
      rubrikergrid[9] = NEW Controls.GridRubrik("WWWFTP","Om ? ska det inte tas bck",TRUE,255).
      */
      
      
      THIS-OBJECT:GridAllaDb:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 9, INPUT THIS-OBJECT:ValdbbuffTTh)). 
      THIS-OBJECT:GridAllaDb:GuruReOpen().
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
      THIS-OBJECT:splitContainerStartLogin = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:PWD = NEW Controls.FillinChar().
      THIS-OBJECT:ANV = NEW Controls.FillinChar().
      THIS-OBJECT:splitContainerAdb = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridAllaDb = NEW Controls.Grid().
      THIS-OBJECT:ServersEdit = NEW Infragistics.Win.UltraWinEditors.UltraTextEditor().
      CAST(THIS-OBJECT:splitContainerStartLogin, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerStartLogin:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerStartLogin:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerStartLogin:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerAdb, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerAdb:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerAdb:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerAdb:SuspendLayout().
      CAST(THIS-OBJECT:GridAllaDb, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:ServersEdit, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* splitContainerStartLogin */
      /*  */
      THIS-OBJECT:splitContainerStartLogin:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerStartLogin:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerStartLogin:Name = "splitContainerStartLogin".
      THIS-OBJECT:splitContainerStartLogin:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerStartLogin.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerStartLogin:Panel1:Controls:Add(THIS-OBJECT:PWD).
      THIS-OBJECT:splitContainerStartLogin:Panel1:Controls:Add(THIS-OBJECT:ANV).
      /*  */
      /* splitContainerStartLogin.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerStartLogin:Panel2:Controls:Add(THIS-OBJECT:splitContainerAdb).
      THIS-OBJECT:splitContainerStartLogin:Size = NEW System.Drawing.Size(1686, 965).
      THIS-OBJECT:splitContainerStartLogin:SplitterDistance = 482.
      THIS-OBJECT:splitContainerStartLogin:TabIndex = 3.
      /*  */
      /* PWD */
      /*  */
      THIS-OBJECT:PWD:Location = NEW System.Drawing.Point(407, 12).
      THIS-OBJECT:PWD:Name = "PWD".
      THIS-OBJECT:PWD:Rubrik = "GuruLabel".
      THIS-OBJECT:PWD:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:PWD:TabIndex = 1.
      THIS-OBJECT:PWD:Leave:Subscribe(THIS-OBJECT:PWD_Leave).
      /*  */
      /* ANV */
      /*  */
      THIS-OBJECT:ANV:Location = NEW System.Drawing.Point(103, 12).
      THIS-OBJECT:ANV:Name = "ANV".
      THIS-OBJECT:ANV:Rubrik = "GuruLabel".
      THIS-OBJECT:ANV:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:ANV:TabIndex = 0.
      /*  */
      /* splitContainerAdb */
      /*  */
      THIS-OBJECT:splitContainerAdb:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerAdb:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerAdb:Name = "splitContainerAdb".
      /*  */
      /* splitContainerAdb.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerAdb:Panel1:Controls:Add(THIS-OBJECT:GridAllaDb).
      /*  */
      /* splitContainerAdb.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerAdb:Panel2:Controls:Add(THIS-OBJECT:ServersEdit).
      THIS-OBJECT:splitContainerAdb:Size = NEW System.Drawing.Size(1686, 479).
      THIS-OBJECT:splitContainerAdb:SplitterDistance = 1335.
      THIS-OBJECT:splitContainerAdb:TabIndex = 1.
      /*  */
      /* GridAllaDb */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridAllaDb:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:GridAllaDb:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridAllaDb:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridAllaDb:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridAllaDb:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:GridAllaDb:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridAllaDb:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:GridAllaDb:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridAllaDb:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridAllaDb:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridAllaDb:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridAllaDb:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:GridAllaDb:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridAllaDb:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridAllaDb:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridAllaDb:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:GridAllaDb:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridAllaDb:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridAllaDb:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:GridAllaDb:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:GridAllaDb:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridAllaDb:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridAllaDb:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:GridAllaDb:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridAllaDb:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:GridAllaDb:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridAllaDb:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridAllaDb:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridAllaDb:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridAllaDb:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridAllaDb:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridAllaDb:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridAllaDb:GuruContext = ?.
      THIS-OBJECT:GridAllaDb:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridAllaDb:Name = "GridAllaDb".
      THIS-OBJECT:GridAllaDb:Size = NEW System.Drawing.Size(1335, 479).
      THIS-OBJECT:GridAllaDb:TabIndex = 1.
      THIS-OBJECT:GridAllaDb:Text = "grid1".
      /*  */
      /* ServersEdit */
      /*  */
      THIS-OBJECT:ServersEdit:AcceptsReturn = TRUE.
      THIS-OBJECT:ServersEdit:AcceptsTab = TRUE.
      THIS-OBJECT:ServersEdit:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:ServersEdit:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:ServersEdit:Multiline = TRUE.
      THIS-OBJECT:ServersEdit:Name = "ServersEdit".
      THIS-OBJECT:ServersEdit:Size = NEW System.Drawing.Size(347, 479).
      THIS-OBJECT:ServersEdit:TabIndex = 33.
      THIS-OBJECT:ServersEdit:Text = " Våra Servers".
      /*  */
      /* AllaDatabaser */
      /*  */
      THIS-OBJECT:ClientSize = NEW System.Drawing.Size(1686, 965).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerStartLogin).
      THIS-OBJECT:Name = "AllaDatabaser".
      THIS-OBJECT:Text = "AllaDatabaser".
      THIS-OBJECT:splitContainerStartLogin:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerStartLogin:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerStartLogin, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerStartLogin:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerAdb:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerAdb:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerAdb:Panel2:PerformLayout().
      CAST(THIS-OBJECT:splitContainerAdb, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerAdb:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridAllaDb, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:ServersEdit, System.ComponentModel.ISupportInitialize):EndInit().
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
	METHOD PRIVATE VOID PWD_Leave( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      kolllosen = CHR(75) + CHR(65) + CHR(71) + CHR(71) + CHR(69) + CHR(78).
      IF THIS-OBJECT:ANV:GuruText:TEXT = CHR(101) + CHR(108) + CHR(112) + CHR(97) + CHR(111) AND kolllosen = THIS-OBJECT:PWD:GuruText:TEXT THEN DO:
         THIS-OBJECT:splitContainerStartLogin:Panel1Collapsed = TRUE.   
         THIS-OBJECT:splitContainerStartLogin:Panel2Collapsed = FALSE.
      END.   
      
   	 
		RETURN.

	END METHOD.

	DESTRUCTOR PUBLIC AllaDatabaser ( ):

	END DESTRUCTOR.

END CLASS.