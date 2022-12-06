 
 /*------------------------------------------------------------------------
    File        : KalkylAdm
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Mon Jan 07 13:32:37 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Guru.Module.
USING Progress.Windows.UserControl.
USING Infragistics.Win.UltraWinGrid.UltraGridRow.

 

CLASS Modules.Kalkyl.CKalkylAdm INHERITS UserControl: 
   
   DEFINE PUBLIC VARIABLE ArbetskoderTTh        AS HANDLE NO-UNDO.
   DEFINE VARIABLE beforekolnamn  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE beforekolvarde AS CHARACTER NO-UNDO.
   /*start*/
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC  VARIABLE ModuleHandle                 AS Guru.Module                                                    NO-UNDO.
   DEFINE PRIVATE VARIABLE HmtRubrikerlista            AS Controls.GridRubrikLista                                        NO-UNDO.
   DEFINE PRIVATE VARIABLE DontCheck                    AS LOGICAL                                                        INITIAL FALSE.
   /* Global */
   DEFINE PUBLIC  VARIABLE GuruDefaultsTTh              AS HANDLE                                                         NO-UNDO.
   
   /*grids*/
   DEFINE PRIVATE VARIABLE GridArbKoder AS Controls.Grid NO-UNDO.
	/*fönster*/   
   DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerBildSek AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerAnvSek AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerKalkKomAnm AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerKatBildDel AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerKatHuvKop AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerKnappAnvSek AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerKatSek AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerLopsubBild AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerLopnr AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerGrundKoder AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerLopSubFrek AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerNyKopKat AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerNyKopKoder AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerNyKopLopnr AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerNyKopSubKat AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerPris AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerKatalogSubKat AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerKatalog AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerPrisKopDel AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerSubKatDel AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerSubKatKoder AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerSubKatPrisDel AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerVisningBild AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE TabSek AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabVisa AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabKoder AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabKataloger AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabDelKataloger AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PRIVATE VARIABLE TabKalkylAdm AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   
  
	CONSTRUCTOR PUBLIC CKalkylAdm ( INPUT r AS Guru.Root ):
	   SUPER().
	   DEBUGGER:SET-BREAK().
      THIS-OBJECT:Root = r.
      THIS-OBJECT:HmtRubrikerlista = NEW Controls.GridRubrikLista().
      InitializeComponent().
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:InitializeTTs().
      THIS-OBJECT:ArbetskoderTTh = THIS-OBJECT:Root:DatabaseManager:KalkylAdm:ArbetskoderTTh.
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
       
	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID GridArbKoder_BeforeCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.BeforeCellUpdateEventArgs ):
		beforekolnamn = e:Cell:Column:ToString().        
      beforekolvarde = e:Cell:VALUE:ToString().
		RETURN.
	END METHOD.





	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	
   METHOD PUBLIC VOID KatalogAfterBort():
      THIS-OBJECT:Root:WindowManager:Wnd:SetCursor(FALSE). 
      RETURN.
   END METHOD.
	@VisualDesigner.
	METHOD PRIVATE VOID GridArbKoder_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
		 /*
      IF THIS-OBJECT:KatalogDeltth:BUFFER-FIELD("UPPDATBAR"):BUFFER-VALUE = FALSE THEN RETURN.
      */
      DEBUGGER:SET-BREAK().
		DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      DEFINE VARIABLE inklogsubid AS INTEGER NO-UNDO.
      DEFINE VARIABLE inrecid AS RECID NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().
      IF kolnamn = "ARBKOD" THEN THIS-OBJECT:ArbetskoderTTh:FIND-FIRST("WHERE ARBKOD = '" + beforekolvarde + "'") NO-ERROR.
      ELSE THIS-OBJECT:ArbetskoderTTh:FIND-FIRST("WHERE ARBKOD = '" + STRING(THIS-OBJECT:GridArbKoder:ActiveRow:Cells["ARBKOD"]:VALUE) + "'") NO-ERROR.
      IF THIS-OBJECT:ArbetskoderTTh:AVAILABLE THEN DO:
         inklogsubid = THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE.
         inrecid = THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE.
      END.
      ELSE RETURN.   
      IF kolnamn = "ARBKOD" THEN DO:
         IF kolvarde = "< Ny kod >" OR kolvarde = "" THEN DO:
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:Root:LanguageManager:GetStringAsMessage(62)," ", System.Windows.Forms.MessageBoxButtons:OK, System.Windows.Forms.MessageBoxIcon:Error).
            THIS-OBJECT:GridArbKoder:ActiveRow:Cells["ARBKOD"]:VALUE = beforekolvarde.
            RETURN.
         END.  
         ELSE DO:
            IF beforekolnamn = kolnamn AND beforekolvarde = kolvarde THEN RETURN.
            
               THIS-OBJECT:ArbetskoderTTh:FIND-FIRST("WHERE ARBKOD = '" + beforekolvarde + "'") NO-ERROR.
               THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.
               MESSAGE STRING(THIS-OBJECT:ArbetskoderTTh:ROWID)
               VIEW-AS ALERT-BOX.
               THIS-OBJECT:GridArbKoder:GuruTom().
               THIS-OBJECT:RefreshArbKoder(?,?).
               DEBUGGER:SET-BREAK().
               THIS-OBJECT:ArbetskoderTTh:FIND-FIRST("WHERE ARBKOD = '" + kolvarde + "'") NO-ERROR.
               THIS-OBJECT:GridArbKoder:GuruRepositionTo(THIS-OBJECT:ArbetskoderTTh:ROWID).             
                                
         END.    
      END.
      ELSE DO:   
         IF beforekolnamn = kolnamn AND beforekolvarde = kolvarde THEN RETURN.
         THIS-OBJECT:ArbetskoderTTh:BUFFER-FIELD(kolnamn):BUFFER-VALUE = kolvarde.             
      END.
		
	END METHOD.













   METHOD PUBLIC LOGICAL Initialize():
      THIS-OBJECT:CreateForm().  
      RETURN TRUE.
   END METHOD.
   METHOD PRIVATE VOID InitializeComponent(  ):
		
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance20 AS Infragistics.Win.Appearance NO-UNDO.
      appearance20 = NEW Infragistics.Win.Appearance().
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
      DEFINE VARIABLE appearance28 AS Infragistics.Win.Appearance NO-UNDO.
      appearance28 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance23 AS Infragistics.Win.Appearance NO-UNDO.
      appearance23 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance22 AS Infragistics.Win.Appearance NO-UNDO.
      appearance22 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance21 AS Infragistics.Win.Appearance NO-UNDO.
      appearance21 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance25 AS Infragistics.Win.Appearance NO-UNDO.
      appearance25 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance27 AS Infragistics.Win.Appearance NO-UNDO.
      appearance27 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance26 AS Infragistics.Win.Appearance NO-UNDO.
      appearance26 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance24 AS Infragistics.Win.Appearance NO-UNDO.
      appearance24 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.Kalkyl.CKalkylAdm").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab3 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab3 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance197 AS Infragistics.Win.Appearance NO-UNDO.
      appearance197 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance198 AS Infragistics.Win.Appearance NO-UNDO.
      appearance198 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab2 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab2 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance199 AS Infragistics.Win.Appearance NO-UNDO.
      appearance199 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab4 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab4 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab5 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab5 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance200 AS Infragistics.Win.Appearance NO-UNDO.
      appearance200 = NEW Infragistics.Win.Appearance().
      THIS-OBJECT:TabKoder = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerGrundKoder = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerSubKatKoder = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerNyKopKoder = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:GridArbKoder = NEW Controls.Grid().
      THIS-OBJECT:splitContainerLopnr = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerNyKopLopnr = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerLopsubBild = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerLopSubFrek = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKalkKomAnm = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:TabKataloger = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerKatalog = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKatalogSubKat = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerNyKopKat = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerPris = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerVisningBild = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:TabDelKataloger = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerSubKatDel = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerSubKatPrisDel = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerNyKopSubKat = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerPrisKopDel = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKatHuvKop = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKatBildDel = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:TabVisa = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabSek = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerKatSek = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerAnvSek = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerKnappAnvSek = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerBildSek = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:TabKalkylAdm = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:TabKoder:SuspendLayout().
      THIS-OBJECT:splitContainerGrundKoder:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerGrundKoder:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerGrundKoder:SuspendLayout().
      THIS-OBJECT:splitContainerSubKatKoder:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerSubKatKoder:SuspendLayout().
      THIS-OBJECT:splitContainerNyKopKoder:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerNyKopKoder:SuspendLayout().
      CAST(THIS-OBJECT:GridArbKoder, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerLopnr:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerLopnr:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerLopnr:SuspendLayout().
      THIS-OBJECT:splitContainerNyKopLopnr:SuspendLayout().
      THIS-OBJECT:splitContainerLopsubBild:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerLopsubBild:SuspendLayout().
      THIS-OBJECT:splitContainerLopSubFrek:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerLopSubFrek:SuspendLayout().
      THIS-OBJECT:splitContainerKalkKomAnm:SuspendLayout().
      THIS-OBJECT:TabKataloger:SuspendLayout().
      THIS-OBJECT:splitContainerKatalog:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKatalog:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKatalog:SuspendLayout().
      THIS-OBJECT:splitContainerKatalogSubKat:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerKatalogSubKat:SuspendLayout().
      THIS-OBJECT:splitContainerNyKopKat:SuspendLayout().
      THIS-OBJECT:splitContainerPris:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerPris:SuspendLayout().
      THIS-OBJECT:splitContainerVisningBild:SuspendLayout().
      THIS-OBJECT:TabDelKataloger:SuspendLayout().
      THIS-OBJECT:splitContainerSubKatDel:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerSubKatDel:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerSubKatDel:SuspendLayout().
      THIS-OBJECT:splitContainerSubKatPrisDel:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerSubKatPrisDel:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerSubKatPrisDel:SuspendLayout().
      THIS-OBJECT:splitContainerNyKopSubKat:SuspendLayout().
      THIS-OBJECT:splitContainerPrisKopDel:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerPrisKopDel:SuspendLayout().
      THIS-OBJECT:splitContainerKatHuvKop:SuspendLayout().
      THIS-OBJECT:splitContainerKatBildDel:SuspendLayout().
      THIS-OBJECT:TabSek:SuspendLayout().
      THIS-OBJECT:splitContainerKatSek:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKatSek:SuspendLayout().
      THIS-OBJECT:splitContainerAnvSek:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerAnvSek:SuspendLayout().
      THIS-OBJECT:splitContainerKnappAnvSek:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerKnappAnvSek:SuspendLayout().
      THIS-OBJECT:splitContainerBildSek:SuspendLayout().
      CAST(THIS-OBJECT:TabKalkylAdm, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabKalkylAdm:SuspendLayout().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* TabKoder */
      /*  */
      THIS-OBJECT:TabKoder:Controls:Add(THIS-OBJECT:splitContainerGrundKoder).
      THIS-OBJECT:TabKoder:Location = NEW System.Drawing.Point(1, 1).
      THIS-OBJECT:TabKoder:Name = "TabKoder".
      THIS-OBJECT:TabKoder:Size = NEW System.Drawing.Size(975, 307).
      /*  */
      /* splitContainerGrundKoder */
      /*  */
      THIS-OBJECT:splitContainerGrundKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerGrundKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerGrundKoder:Name = "splitContainerGrundKoder".
      /*  */
      /* splitContainerGrundKoder.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerGrundKoder:Panel1:Controls:Add(THIS-OBJECT:splitContainerSubKatKoder).
      /*  */
      /* splitContainerGrundKoder.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerGrundKoder:Panel2:Controls:Add(THIS-OBJECT:splitContainerLopnr).
      THIS-OBJECT:splitContainerGrundKoder:Size = NEW System.Drawing.Size(975, 307).
      THIS-OBJECT:splitContainerGrundKoder:SplitterDistance = 450.
      THIS-OBJECT:splitContainerGrundKoder:TabIndex = 0.
      /*  */
      /* splitContainerSubKatKoder */
      /*  */
      THIS-OBJECT:splitContainerSubKatKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerSubKatKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerSubKatKoder:Name = "splitContainerSubKatKoder".
      /*  */
      /* splitContainerSubKatKoder.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerSubKatKoder:Panel2:Controls:Add(THIS-OBJECT:splitContainerNyKopKoder).
      THIS-OBJECT:splitContainerSubKatKoder:Size = NEW System.Drawing.Size(450, 307).
      THIS-OBJECT:splitContainerSubKatKoder:SplitterDistance = 190.
      THIS-OBJECT:splitContainerSubKatKoder:TabIndex = 0.
      /*  */
      /* splitContainerNyKopKoder */
      /*  */
      THIS-OBJECT:splitContainerNyKopKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerNyKopKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerNyKopKoder:Name = "splitContainerNyKopKoder".
      THIS-OBJECT:splitContainerNyKopKoder:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerNyKopKoder.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerNyKopKoder:Panel1:Controls:Add(THIS-OBJECT:GridArbKoder).
      THIS-OBJECT:splitContainerNyKopKoder:Size = NEW System.Drawing.Size(256, 307).
      THIS-OBJECT:splitContainerNyKopKoder:SplitterDistance = 278.
      THIS-OBJECT:splitContainerNyKopKoder:TabIndex = 2.
      /*  */
      /* GridArbKoder */
      /*  */
      appearance20:BackColor = System.Drawing.SystemColors:Window.
      appearance20:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Appearance = appearance20.
      THIS-OBJECT:GridArbKoder:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:GridArbKoder:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance17:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance17:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance17:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance17:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbKoder:DisplayLayout:GroupByBox:Appearance = appearance17.
      appearance18:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridArbKoder:DisplayLayout:GroupByBox:BandLabelAppearance = appearance18.
      THIS-OBJECT:GridArbKoder:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance19:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance19:BackColor2 = System.Drawing.SystemColors:Control.
      appearance19:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance19:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:GridArbKoder:DisplayLayout:GroupByBox:PromptAppearance = appearance19.
      THIS-OBJECT:GridArbKoder:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:GridArbKoder:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance28:BackColor = System.Drawing.SystemColors:Window.
      appearance28:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:ActiveCellAppearance = appearance28.
      appearance23:BackColor = System.Drawing.SystemColors:Highlight.
      appearance23:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:ActiveRowAppearance = appearance23.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance22:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:CardAreaAppearance = appearance22.
      appearance21:BorderColor = System.Drawing.Color:Silver.
      appearance21:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:CellAppearance = appearance21.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:CellPadding = 0.
      appearance25:BackColor = System.Drawing.SystemColors:Control.
      appearance25:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance25:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance25:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance25:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:GroupByRowAppearance = appearance25.
      appearance27:TextHAlignAsString = "Left".
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:HeaderAppearance = appearance27.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance26:BackColor = System.Drawing.SystemColors:Window.
      appearance26:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:RowAppearance = appearance26.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance24:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:GridArbKoder:DisplayLayout:Override:TemplateAddRowAppearance = appearance24.
      THIS-OBJECT:GridArbKoder:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:GridArbKoder:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:GridArbKoder:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:GridArbKoder:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:GridArbKoder:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:GridArbKoder:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:GridArbKoder:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:GridArbKoder:GuruContext = ?.
      THIS-OBJECT:GridArbKoder:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:GridArbKoder:Name = "GridArbKoder".
      THIS-OBJECT:GridArbKoder:Size = NEW System.Drawing.Size(256, 278).
      THIS-OBJECT:GridArbKoder:TabIndex = 1.
      THIS-OBJECT:GridArbKoder:Text = "Grid1".
      THIS-OBJECT:GridArbKoder:AfterCellUpdate:Subscribe(THIS-OBJECT:GridArbKoder_AfterCellUpdate).
      THIS-OBJECT:GridArbKoder:BeforeCellUpdate:Subscribe(THIS-OBJECT:GridArbKoder_BeforeCellUpdate).
      /*  */
      /* splitContainerLopnr */
      /*  */
      THIS-OBJECT:splitContainerLopnr:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerLopnr:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerLopnr:Name = "splitContainerLopnr".
      /*  */
      /* splitContainerLopnr.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerLopnr:Panel1:Controls:Add(THIS-OBJECT:splitContainerNyKopLopnr).
      /*  */
      /* splitContainerLopnr.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerLopnr:Panel2:Controls:Add(THIS-OBJECT:splitContainerLopsubBild).
      THIS-OBJECT:splitContainerLopnr:Size = NEW System.Drawing.Size(521, 307).
      THIS-OBJECT:splitContainerLopnr:SplitterDistance = 200.
      THIS-OBJECT:splitContainerLopnr:TabIndex = 0.
      /*  */
      /* splitContainerNyKopLopnr */
      /*  */
      THIS-OBJECT:splitContainerNyKopLopnr:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerNyKopLopnr:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerNyKopLopnr:Name = "splitContainerNyKopLopnr".
      THIS-OBJECT:splitContainerNyKopLopnr:Orientation = System.Windows.Forms.Orientation:Horizontal.
      THIS-OBJECT:splitContainerNyKopLopnr:Size = NEW System.Drawing.Size(200, 307).
      THIS-OBJECT:splitContainerNyKopLopnr:SplitterDistance = 278.
      THIS-OBJECT:splitContainerNyKopLopnr:TabIndex = 2.
      /*  */
      /* splitContainerLopsubBild */
      /*  */
      THIS-OBJECT:splitContainerLopsubBild:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerLopsubBild:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerLopsubBild:Name = "splitContainerLopsubBild".
      /*  */
      /* splitContainerLopsubBild.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerLopsubBild:Panel1:Controls:Add(THIS-OBJECT:splitContainerLopSubFrek).
      /*  */
      /* splitContainerLopsubBild.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerLopsubBild:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerLopsubBild.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerLopsubBild:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Center.
      THIS-OBJECT:splitContainerLopsubBild:Size = NEW System.Drawing.Size(317, 307).
      THIS-OBJECT:splitContainerLopsubBild:SplitterDistance = 240.
      THIS-OBJECT:splitContainerLopsubBild:TabIndex = 0.
      /*  */
      /* splitContainerLopSubFrek */
      /*  */
      THIS-OBJECT:splitContainerLopSubFrek:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerLopSubFrek:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerLopSubFrek:Name = "splitContainerLopSubFrek".
      /*  */
      /* splitContainerLopSubFrek.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerLopSubFrek:Panel2:Controls:Add(THIS-OBJECT:splitContainerKalkKomAnm).
      THIS-OBJECT:splitContainerLopSubFrek:Size = NEW System.Drawing.Size(240, 307).
      THIS-OBJECT:splitContainerLopSubFrek:SplitterDistance = 173.
      THIS-OBJECT:splitContainerLopSubFrek:TabIndex = 3.
      /*  */
      /* splitContainerKalkKomAnm */
      /*  */
      THIS-OBJECT:splitContainerKalkKomAnm:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKalkKomAnm:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKalkKomAnm:Name = "splitContainerKalkKomAnm".
      THIS-OBJECT:splitContainerKalkKomAnm:Orientation = System.Windows.Forms.Orientation:Horizontal.
      THIS-OBJECT:splitContainerKalkKomAnm:Size = NEW System.Drawing.Size(63, 307).
      THIS-OBJECT:splitContainerKalkKomAnm:SplitterDistance = 34.
      THIS-OBJECT:splitContainerKalkKomAnm:TabIndex = 0.
      /*  */
      /* TabKataloger */
      /*  */
      THIS-OBJECT:TabKataloger:Controls:Add(THIS-OBJECT:splitContainerKatalog).
      THIS-OBJECT:TabKataloger:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabKataloger:Name = "TabKataloger".
      THIS-OBJECT:TabKataloger:Size = NEW System.Drawing.Size(975, 307).
      /*  */
      /* splitContainerKatalog */
      /*  */
      THIS-OBJECT:splitContainerKatalog:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKatalog:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKatalog:Name = "splitContainerKatalog".
      /*  */
      /* splitContainerKatalog.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKatalog:Panel1:Controls:Add(THIS-OBJECT:splitContainerKatalogSubKat).
      /*  */
      /* splitContainerKatalog.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKatalog:Panel2:Controls:Add(THIS-OBJECT:splitContainerPris).
      THIS-OBJECT:splitContainerKatalog:Size = NEW System.Drawing.Size(975, 307).
      THIS-OBJECT:splitContainerKatalog:SplitterDistance = 500.
      THIS-OBJECT:splitContainerKatalog:TabIndex = 0.
      /*  */
      /* splitContainerKatalogSubKat */
      /*  */
      THIS-OBJECT:splitContainerKatalogSubKat:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKatalogSubKat:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKatalogSubKat:Name = "splitContainerKatalogSubKat".
      /*  */
      /* splitContainerKatalogSubKat.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerKatalogSubKat:Panel1:Controls:Add(THIS-OBJECT:splitContainerNyKopKat).
      THIS-OBJECT:splitContainerKatalogSubKat:Size = NEW System.Drawing.Size(500, 307).
      THIS-OBJECT:splitContainerKatalogSubKat:SplitterDistance = 250.
      THIS-OBJECT:splitContainerKatalogSubKat:TabIndex = 0.
      /*  */
      /* splitContainerNyKopKat */
      /*  */
      THIS-OBJECT:splitContainerNyKopKat:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerNyKopKat:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerNyKopKat:Name = "splitContainerNyKopKat".
      THIS-OBJECT:splitContainerNyKopKat:Orientation = System.Windows.Forms.Orientation:Horizontal.
      THIS-OBJECT:splitContainerNyKopKat:Size = NEW System.Drawing.Size(250, 307).
      THIS-OBJECT:splitContainerNyKopKat:SplitterDistance = 278.
      THIS-OBJECT:splitContainerNyKopKat:TabIndex = 1.
      /*  */
      /* splitContainerPris */
      /*  */
      THIS-OBJECT:splitContainerPris:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerPris:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerPris:Name = "splitContainerPris".
      /*  */
      /* splitContainerPris.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerPris:Panel2:Controls:Add(THIS-OBJECT:splitContainerVisningBild).
      THIS-OBJECT:splitContainerPris:Size = NEW System.Drawing.Size(471, 307).
      THIS-OBJECT:splitContainerPris:SplitterDistance = 231.
      THIS-OBJECT:splitContainerPris:TabIndex = 0.
      /*  */
      /* splitContainerVisningBild */
      /*  */
      THIS-OBJECT:splitContainerVisningBild:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerVisningBild:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerVisningBild:Name = "splitContainerVisningBild".
      /*  */
      /* splitContainerVisningBild.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerVisningBild:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerVisningBild.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerVisningBild:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Center.
      THIS-OBJECT:splitContainerVisningBild:Size = NEW System.Drawing.Size(236, 307).
      THIS-OBJECT:splitContainerVisningBild:SplitterDistance = 205.
      THIS-OBJECT:splitContainerVisningBild:TabIndex = 0.
      /*  */
      /* TabDelKataloger */
      /*  */
      THIS-OBJECT:TabDelKataloger:Controls:Add(THIS-OBJECT:splitContainerSubKatDel).
      THIS-OBJECT:TabDelKataloger:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabDelKataloger:Name = "TabDelKataloger".
      THIS-OBJECT:TabDelKataloger:Size = NEW System.Drawing.Size(975, 307).
      /*  */
      /* splitContainerSubKatDel */
      /*  */
      THIS-OBJECT:splitContainerSubKatDel:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerSubKatDel:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerSubKatDel:Name = "splitContainerSubKatDel".
      /*  */
      /* splitContainerSubKatDel.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerSubKatDel:Panel1:Controls:Add(THIS-OBJECT:splitContainerSubKatPrisDel).
      /*  */
      /* splitContainerSubKatDel.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerSubKatDel:Panel2:Controls:Add(THIS-OBJECT:splitContainerKatBildDel).
      THIS-OBJECT:splitContainerSubKatDel:Size = NEW System.Drawing.Size(975, 307).
      THIS-OBJECT:splitContainerSubKatDel:SplitterDistance = 600.
      THIS-OBJECT:splitContainerSubKatDel:TabIndex = 0.
      /*  */
      /* splitContainerSubKatPrisDel */
      /*  */
      THIS-OBJECT:splitContainerSubKatPrisDel:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerSubKatPrisDel:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerSubKatPrisDel:Name = "splitContainerSubKatPrisDel".
      /*  */
      /* splitContainerSubKatPrisDel.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerSubKatPrisDel:Panel1:Controls:Add(THIS-OBJECT:splitContainerNyKopSubKat).
      /*  */
      /* splitContainerSubKatPrisDel.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerSubKatPrisDel:Panel2:Controls:Add(THIS-OBJECT:splitContainerPrisKopDel).
      THIS-OBJECT:splitContainerSubKatPrisDel:Size = NEW System.Drawing.Size(600, 307).
      THIS-OBJECT:splitContainerSubKatPrisDel:SplitterDistance = 196.
      THIS-OBJECT:splitContainerSubKatPrisDel:TabIndex = 0.
      /*  */
      /* splitContainerNyKopSubKat */
      /*  */
      THIS-OBJECT:splitContainerNyKopSubKat:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerNyKopSubKat:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerNyKopSubKat:Name = "splitContainerNyKopSubKat".
      THIS-OBJECT:splitContainerNyKopSubKat:Orientation = System.Windows.Forms.Orientation:Horizontal.
      THIS-OBJECT:splitContainerNyKopSubKat:Size = NEW System.Drawing.Size(196, 307).
      THIS-OBJECT:splitContainerNyKopSubKat:SplitterDistance = 278.
      THIS-OBJECT:splitContainerNyKopSubKat:TabIndex = 1.
      /*  */
      /* splitContainerPrisKopDel */
      /*  */
      THIS-OBJECT:splitContainerPrisKopDel:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerPrisKopDel:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerPrisKopDel:Name = "splitContainerPrisKopDel".
      /*  */
      /* splitContainerPrisKopDel.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerPrisKopDel:Panel2:Controls:Add(THIS-OBJECT:splitContainerKatHuvKop).
      THIS-OBJECT:splitContainerPrisKopDel:Size = NEW System.Drawing.Size(400, 307).
      THIS-OBJECT:splitContainerPrisKopDel:SplitterDistance = 238.
      THIS-OBJECT:splitContainerPrisKopDel:TabIndex = 2.
      /*  */
      /* splitContainerKatHuvKop */
      /*  */
      THIS-OBJECT:splitContainerKatHuvKop:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKatHuvKop:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKatHuvKop:Name = "splitContainerKatHuvKop".
      THIS-OBJECT:splitContainerKatHuvKop:Size = NEW System.Drawing.Size(158, 307).
      THIS-OBJECT:splitContainerKatHuvKop:SplitterDistance = 129.
      THIS-OBJECT:splitContainerKatHuvKop:TabIndex = 0.
      /*  */
      /* splitContainerKatBildDel */
      /*  */
      THIS-OBJECT:splitContainerKatBildDel:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKatBildDel:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKatBildDel:Name = "splitContainerKatBildDel".
      /*  */
      /* splitContainerKatBildDel.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKatBildDel:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerKatBildDel.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerKatBildDel:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Center.
      THIS-OBJECT:splitContainerKatBildDel:Size = NEW System.Drawing.Size(371, 307).
      THIS-OBJECT:splitContainerKatBildDel:SplitterDistance = 251.
      THIS-OBJECT:splitContainerKatBildDel:TabIndex = 0.
      /*  */
      /* TabVisa */
      /*  */
      THIS-OBJECT:TabVisa:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabVisa:Name = "TabVisa".
      THIS-OBJECT:TabVisa:Size = NEW System.Drawing.Size(975, 307).
      /*  */
      /* TabSek */
      /*  */
      THIS-OBJECT:TabSek:Controls:Add(THIS-OBJECT:splitContainerKatSek).
      THIS-OBJECT:TabSek:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabSek:Name = "TabSek".
      THIS-OBJECT:TabSek:Size = NEW System.Drawing.Size(975, 307).
      /*  */
      /* splitContainerKatSek */
      /*  */
      THIS-OBJECT:splitContainerKatSek:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKatSek:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKatSek:Name = "splitContainerKatSek".
      /*  */
      /* splitContainerKatSek.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKatSek:Panel2:Controls:Add(THIS-OBJECT:splitContainerAnvSek).
      THIS-OBJECT:splitContainerKatSek:Size = NEW System.Drawing.Size(975, 307).
      THIS-OBJECT:splitContainerKatSek:SplitterDistance = 350.
      THIS-OBJECT:splitContainerKatSek:TabIndex = 0.
      /*  */
      /* splitContainerAnvSek */
      /*  */
      THIS-OBJECT:splitContainerAnvSek:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerAnvSek:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerAnvSek:Name = "splitContainerAnvSek".
      /*  */
      /* splitContainerAnvSek.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerAnvSek:Panel2:Controls:Add(THIS-OBJECT:splitContainerKnappAnvSek).
      THIS-OBJECT:splitContainerAnvSek:Size = NEW System.Drawing.Size(621, 307).
      THIS-OBJECT:splitContainerAnvSek:SplitterDistance = 205.
      THIS-OBJECT:splitContainerAnvSek:TabIndex = 0.
      /*  */
      /* splitContainerKnappAnvSek */
      /*  */
      THIS-OBJECT:splitContainerKnappAnvSek:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerKnappAnvSek:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerKnappAnvSek:Name = "splitContainerKnappAnvSek".
      /*  */
      /* splitContainerKnappAnvSek.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerKnappAnvSek:Panel2:Controls:Add(THIS-OBJECT:splitContainerBildSek).
      THIS-OBJECT:splitContainerKnappAnvSek:Size = NEW System.Drawing.Size(412, 307).
      THIS-OBJECT:splitContainerKnappAnvSek:SplitterDistance = 33.
      THIS-OBJECT:splitContainerKnappAnvSek:TabIndex = 0.
      /*  */
      /* splitContainerBildSek */
      /*  */
      THIS-OBJECT:splitContainerBildSek:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerBildSek:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerBildSek:Name = "splitContainerBildSek".
      /*  */
      /* splitContainerBildSek.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerBildSek:Panel2:BackgroundImage = CAST(resources:GetObject("splitContainerBildSek.Panel2.BackgroundImage"), System.Drawing.Image).
      THIS-OBJECT:splitContainerBildSek:Panel2:BackgroundImageLayout = System.Windows.Forms.ImageLayout:Center.
      THIS-OBJECT:splitContainerBildSek:Size = NEW System.Drawing.Size(375, 307).
      THIS-OBJECT:splitContainerBildSek:SplitterDistance = 200.
      THIS-OBJECT:splitContainerBildSek:TabIndex = 0.
      /*  */
      /* TabKalkylAdm */
      /*  */
      THIS-OBJECT:TabKalkylAdm:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
      THIS-OBJECT:TabKalkylAdm:Controls:Add(THIS-OBJECT:TabKataloger).
      THIS-OBJECT:TabKalkylAdm:Controls:Add(THIS-OBJECT:TabDelKataloger).
      THIS-OBJECT:TabKalkylAdm:Controls:Add(THIS-OBJECT:TabKoder).
      THIS-OBJECT:TabKalkylAdm:Controls:Add(THIS-OBJECT:TabVisa).
      THIS-OBJECT:TabKalkylAdm:Controls:Add(THIS-OBJECT:TabSek).
      THIS-OBJECT:TabKalkylAdm:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:TabKalkylAdm:ImageSize = NEW System.Drawing.Size(40, 40).
      THIS-OBJECT:TabKalkylAdm:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:TabKalkylAdm:Name = "TabKalkylAdm".
      THIS-OBJECT:TabKalkylAdm:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
      THIS-OBJECT:TabKalkylAdm:Size = NEW System.Drawing.Size(979, 357).
      THIS-OBJECT:TabKalkylAdm:TabIndex = 0.
      THIS-OBJECT:TabKalkylAdm:TabOrientation = Infragistics.Win.UltraWinTabs.TabOrientation:BottomLeft.
      appearance197:Image = CAST(resources:GetObject("appearance197.Image"), System.Object).
      ultraTab3:Appearance = appearance197.
      ultraTab3:Key = "Koder".
      ultraTab3:TabPage = THIS-OBJECT:TabKoder.
      ultraTab3:Text = "".
      ultraTab3:ToolTipText = "Koder".
      appearance198:Image = CAST(resources:GetObject("appearance198.Image"), System.Object).
      ultraTab1:Appearance = appearance198.
      ultraTab1:Key = "Kataloger".
      ultraTab1:TabPage = THIS-OBJECT:TabKataloger.
      ultraTab1:Text = "".
      ultraTab1:ToolTipText = "Kataloger".
      appearance199:Image = CAST(resources:GetObject("appearance199.Image"), System.Object).
      ultraTab2:Appearance = appearance199.
      ultraTab2:Key = "Delkataloger".
      ultraTab2:TabPage = THIS-OBJECT:TabDelKataloger.
      ultraTab2:Text = "".
      ultraTab2:ToolTipText = "Delkataloger".
      ultraTab4:Key = "Visa".
      ultraTab4:TabPage = THIS-OBJECT:TabVisa.
      ultraTab4:Text = "Visa".
      ultraTab4:ToolTipText = "Visa kataloger".
      appearance200:Image = CAST(resources:GetObject("appearance200.Image"), System.Object).
      ultraTab5:Appearance = appearance200.
      ultraTab5:Key = "Sekretess".
      ultraTab5:TabPage = THIS-OBJECT:TabSek.
      ultraTab5:Text = "".
      ultraTab5:ToolTipText = "Sekretess".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 5 NO-UNDO.
      arrayvar0[1] = ultraTab3.
      arrayvar0[2] = ultraTab1.
      arrayvar0[3] = ultraTab2.
      arrayvar0[4] = ultraTab4.
      arrayvar0[5] = ultraTab5.
      THIS-OBJECT:TabKalkylAdm:Tabs:AddRange(arrayvar0).
      /*  */
      /* ultraTabSharedControlsPage1 */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1".
      THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(975, 307).
      /*  */
      /* KalkylAdm */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(6, 13).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:TabKalkylAdm).
      THIS-OBJECT:Name = "KalkylAdm".
      THIS-OBJECT:Size = NEW System.Drawing.Size(979, 357).
      THIS-OBJECT:TabKoder:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerGrundKoder:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerGrundKoder:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerGrundKoder:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSubKatKoder:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSubKatKoder:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNyKopKoder:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNyKopKoder:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:GridArbKoder, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerLopnr:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerLopnr:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerLopnr:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNyKopLopnr:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerLopsubBild:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerLopsubBild:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerLopSubFrek:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerLopSubFrek:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKalkKomAnm:ResumeLayout(FALSE).
      THIS-OBJECT:TabKataloger:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKatalog:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKatalog:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKatalog:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKatalogSubKat:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKatalogSubKat:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNyKopKat:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerPris:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerPris:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerVisningBild:ResumeLayout(FALSE).
      THIS-OBJECT:TabDelKataloger:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSubKatDel:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSubKatDel:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSubKatDel:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSubKatPrisDel:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSubKatPrisDel:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerSubKatPrisDel:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNyKopSubKat:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerPrisKopDel:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerPrisKopDel:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKatHuvKop:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKatBildDel:ResumeLayout(FALSE).
      THIS-OBJECT:TabSek:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKatSek:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKatSek:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerAnvSek:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerAnvSek:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKnappAnvSek:Panel2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerKnappAnvSek:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerBildSek:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:TabKalkylAdm, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabKalkylAdm:ResumeLayout(FALSE).
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.
   METHOD PUBLIC VOID StartKalkAdm():
      
   END METHOD.   
   METHOD PUBLIC VOID CreateForm():
      THIS-OBJECT:CreateGrids().
      
   END METHOD.
   
    
   METHOD PUBLIC VOID RefreshArbKoder(s AS System.Object, e AS System.EventArgs):
      DEFINE VARIABLE Typ        AS INTEGER   NO-UNDO INITIAL 1.
      DEFINE VARIABLE Markning   AS CHARACTER NO-UNDO.
      DEFINE VARIABLE DoAll      AS LOGICAL   NO-UNDO.
      DEFINE VARIABLE DoUnmarked AS LOGICAL   NO-UNDO.
      THIS-OBJECT:GridArbKoder:GuruFiltrera().
      THIS-OBJECT:GridArbKoder:GuruFirstrow().
      THIS-OBJECT:ValueGridArbKoderCh(?,?).
     
   END METHOD.
   
   METHOD PUBLIC VOID CreateGrids():
      THIS-OBJECT:CreateGridArbKoder().
      THIS-OBJECT:Root:DatabaseManager:KalkylAdm:CFetchKoder().
       THIS-OBJECT:GridArbKoder:GuruReopen().
   END METHOD.
   METHOD PUBLIC VOID CreateGridArbKoder():
      DEFINE VARIABLE rubrikergrid     AS Controls.GridRubrik      NO-UNDO EXTENT 10.
      rubrikergrid[1] = NEW Controls.GridRubrik("ARBKOD","Kod",FALSE,48).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","Benämning",FALSE,230).
      rubrikergrid[3] = NEW Controls.GridRubrik("MARKNING","Märkning",FALSE,64).
      rubrikergrid[4] = NEW Controls.GridRubrik("MARKSUB","Intern kommentar",FALSE,125).
      rubrikergrid[5] = NEW Controls.GridRubrik("TYPKALK","Typ",FALSE,50).
      THIS-OBJECT:GridArbKoder:GuruInitGrid(THIS-OBJECT:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 5, INPUT THIS-OBJECT:ArbetskoderTTh)).  
      GridArbKoder:TEXT = "Arbetskoder".
      GridArbKoder:tBS:PositionChanged:Subscribe(ValueGridArbKoderCh).  
       THIS-OBJECT:GridArbKoder:GuruReopen().          
   END METHOD.
   
   
	

   
   METHOD PRIVATE VOID ValueGridArbKoderCh (INPUT rSender AS System.Object, INPUT rArgs AS System.EventArgs):                  
   END METHOD.
    
	DESTRUCTOR PUBLIC CKalkylAdm ( ):

		IF VALID-OBJECT(components) THEN DO:
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.

END CLASS.