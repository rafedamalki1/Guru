 
 /*------------------------------------------------------------------------
    File        : EkgRPControl
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Fri Oct 11 15:19:04 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Guru.Module.
USING Progress.Windows.UserControl.
USING Infragistics.Win.UltraWinGrid.UltraGridRow.


CLASS Modules.EkgData.EkgRPControl INHERITS UserControl: 
  
   DEFINE PUBLIC VARIABLE buttonOverResurspris AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE buttonOverResursPrisBort AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE laggTillRKButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlShell                  AS Modules.EkgData.EkgShell                                       NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlWinLg                  AS Modules.EkgData.EkgWinLg                                       NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlDataDb                 AS Modules.EkgData.EkgDataDb                                      NO-UNDO.
   
   DEFINE PUBLIC VARIABLE splitContainerBas1 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerBas2 AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPagePaslagNiv AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PRIVATE VARIABLE toolStripNiva AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE toolStripBasresurser AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE taBortResursPrisButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PRIVATE VARIABLE taBortNiva AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerResursPrisLista AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerResursPris AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerPaslagOver AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE splitContainerPaslag AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerNiva AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE splitContainerBasresurser AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE resurspristoolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE resurspaslagMenuSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE resursPaslagtoolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PRIVATE VARIABLE regeltoolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE regelMenuSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE paslagToolStrip AS System.Windows.Forms.ToolStrip NO-UNDO.
   DEFINE PUBLIC VARIABLE paslagMenuSC AS System.Windows.Forms.SplitContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE laggTillNivaButton AS System.Windows.Forms.ToolStripButton NO-UNDO.
   DEFINE PUBLIC VARIABLE gridResurspris AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridResursniva AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridResursPaslag AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridResursForResurspris AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridRegel AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE gridPaslag AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE buttonOverResurspaslagBort AS Controls.ButtonOver NO-UNDO.
   DEFINE PUBLIC VARIABLE buttonOverResurspaslag AS Controls.ButtonOver NO-UNDO.
   DEFINE PRIVATE VARIABLE TabRPRegel AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabRPPaslag AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabRPNiva AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabManagerResurspriser AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   DEFINE PUBLIC  VARIABLE beforekolnamn  AS CHARACTER NO-UNDO.
   
   
  CONSTRUCTOR PUBLIC EkgRPControl (INPUT r AS Guru.Root):
      SUPER().
      
      THIS-OBJECT:Root = r.
      THIS-OBJECT:InitializeComponent().
      THIS-OBJECT:ControlDataDb = THIS-OBJECT:Root:DatabaseManager:EkgData.
      THIS-OBJECT:ControlShell = THIS-OBJECT:Root:DatabaseManager:EkgData:ControlShell.
      THIS-OBJECT:ControlWinLg = THIS-OBJECT:ControlShell:ControlWinLg.
      
      THIS-OBJECT:CreateForm().
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
      
   END CONSTRUCTOR.

   METHOD PUBLIC VOID CreateForm():
     THIS-OBJECT:CreateGrids().
   END METHOD.
   METHOD PUBLIC VOID CreateGrids():
      THIS-OBJECT:CreateGridRegel().
      THIS-OBJECT:CreateGridResurspris().
      THIS-OBJECT:CreateGridResursniva().
      THIS-OBJECT:CreateGridResursForResurspris().
      THIS-OBJECT:CreateGridPaslag().
      THIS-OBJECT:CreateGridResursPaslag().
   END METHOD.
   
   METHOD PUBLIC VOID CreateGridRegel():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("REGELID","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("ANMARKNING","",TRUE,250).
      rubrikergrid[5] = NEW Controls.GridRubrik("TIMSUMKOD","",TRUE,70).
      rubrikergrid[6] = NEW Controls.GridRubrik("EASORTKLG1","",TRUE,70).
      /*rubrikergrid[7] = NEW Controls.GridRubrik("EASORTKLG2","",TRUE,70).*/
      rubrikergrid[7] = NEW Controls.GridRubrik("UTOVERFREK","",TRUE,70, "LOG").
      rubrikergrid[8] = NEW Controls.GridRubrik("EAFAKTOR","",TRUE,70, "LOG").
      
      
      THIS-OBJECT:GridRegel:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 8, INPUT THIS-OBJECT:ControlDataDb:ekgregelTTh)).
      THIS-OBJECT:GridRegel:Text = "Regler".
      
      /*DÖLJ*/
      THIS-OBJECT:GridRegel:DisplayLayout:Bands[0]:Columns["EASORTKLG2"]:Hidden = FALSE.
      THIS-OBJECT:GridRegel:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:GridRegel:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
   END METHOD.

   METHOD PUBLIC VOID CreateGridResurspris():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("RESURSNR","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("RESURSBENAMNING","",TRUE,250).
      rubrikergrid[5] = NEW Controls.GridRubrik("RESURSENHET","",TRUE,50).
      rubrikergrid[6] = NEW Controls.GridRubrik("PRIS","",FALSE,70,"DEC").
      rubrikergrid[7] = NEW Controls.GridRubrik("EA","",TRUE,70,"DEC").
      rubrikergrid[8] = NEW Controls.GridRubrik("REGELID","",FALSE,50,"DEC").
      rubrikergrid[9] = NEW Controls.GridRubrik("REGELANMARKNING","",TRUE,130).
      rubrikergrid[10] = NEW Controls.GridRubrik("RESURSAKTIV","",TRUE,50, "LOG").
  
      THIS-OBJECT:GridResurspris:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 10, INPUT THIS-OBJECT:ControlDataDb:ekgresursprisTTh )).
      THIS-OBJECT:GridResurspris:Text = "Resurskostnad".
      THIS-OBJECT:GridResurspris:tBS:PositionChanged:Subscribe(THIS-OBJECT:ControlWinLg:ValueChangeResursPris).
      /*DÖLJ*/
      /*THIS-OBJECT:ControlShell:GridResurspris:DisplayLayout:Bands[0]:Columns["REGELID"]:Hidden = TRUE.*/
      THIS-OBJECT:GridResurspris:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:GridResurspris:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
   END METHOD. 
   
   METHOD PUBLIC VOID CreateGridResursniva():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("RESURSNR","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("RESURSBENAMNING","",TRUE,250).
      rubrikergrid[5] = NEW Controls.GridRubrik("NIVA","",TRUE,70).
      /*rubrikergrid[6] = NEW Controls.GridRubrik("FORVALDEJFREK","",FALSE,50, "LOG").
      rubrikergrid[7] = NEW Controls.GridRubrik("FORVALDFREK","",FALSE,50, "LOG").*/

      THIS-OBJECT:GridResursniva:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 5, INPUT THIS-OBJECT:ControlDataDb:ekgresursnivaTTh)).
      THIS-OBJECT:GridResursniva:Text = "Resursnivåer".
           
      
      /*DÖLJ EKGSUBID O ERBKAT*/
      THIS-OBJECT:GridResursniva:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:GridResursniva:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
   END METHOD. 
   
   METHOD PUBLIC VOID CreateGridResursForResurspris():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("RESURSNR","",TRUE,100).
      rubrikergrid[2] = NEW Controls.GridRubrik("BENAMNING","",TRUE,250).
      rubrikergrid[3] = NEW Controls.GridRubrik("ENHET","",TRUE,50).
      
      THIS-OBJECT:GridResursForResurspris:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT FALSE,INPUT 3, INPUT THIS-OBJECT:ControlDataDb:ekgresursTTh )).
      THIS-OBJECT:GridResursForResurspris:Text = "Resurser".
   END METHOD. 
   
   METHOD PUBLIC VOID CreateGridPaslag():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("PASLAGNR","",TRUE,40).
      rubrikergrid[4] = NEW Controls.GridRubrik("BENAMNING","",FALSE,200).
      rubrikergrid[5] = NEW Controls.GridRubrik("PASLAG","",FALSE,70,"DEC").
      
      THIS-OBJECT:GridPaslag:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 5, INPUT THIS-OBJECT:ControlDataDb:ekgpaslagTTh)).
      THIS-OBJECT:GridPaslag:Text = "Påslag".
      
      /*DÖLJ EKGSUBID O ERBKAT*/
      THIS-OBJECT:GridPaslag:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:GridPaslag:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
   END METHOD. 
   
   METHOD PUBLIC VOID CreateGridResursPaslag():
      {GridRubrikListaVarExtent.i}
      rubrikergrid[1] = NEW Controls.GridRubrik("EKGSUBID","",TRUE,70).
      rubrikergrid[2] = NEW Controls.GridRubrik("EBRKAT","",TRUE,70).
      rubrikergrid[3] = NEW Controls.GridRubrik("RESURSNR","",TRUE,70).
      rubrikergrid[4] = NEW Controls.GridRubrik("RESURSBENAMNING","",TRUE,250).
      rubrikergrid[5] = NEW Controls.GridRubrik("PASLAGNR","",TRUE,70).
      rubrikergrid[6] = NEW Controls.GridRubrik("PASLAGSBENAMNING","",TRUE,250).
        
      THIS-OBJECT:GridResursPaslag:GuruInitGrid(THIS-OBJECT:ControlWinLg:HmtRubrikerlista:createRubrik(INPUT rubrikergrid,INPUT TRUE,INPUT 6, INPUT THIS-OBJECT:ControlDataDb:ekgresurspaslagTTh)).
      THIS-OBJECT:GridResursPaslag:Text = "Resurspåslag".
      
      /*DÖLJ*/
      THIS-OBJECT:GridResursPaslag:DisplayLayout:Bands[0]:Columns["EKGSUBID"]:Hidden = TRUE.
      THIS-OBJECT:GridResursPaslag:DisplayLayout:Bands[0]:Columns["EBRKAT"]:Hidden = TRUE. 
   END METHOD.  
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonOverResursPrisBort_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:TaBortResurspris(INTEGER(THIS-OBJECT:GridResurspris:ActiveRow:Cells["RESURSNR"]:Text)).
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonOverResurspris_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:NyResurspris().
      RETURN.

   END METHOD.
     
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonOverResurspaslagBort_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:buttonOverResurspaslagBort(). 
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID buttonOverResurspaslag_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:buttonOverResurspaslag().
      RETURN.
   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridregel_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().     
      IF THIS-OBJECT:ControlWinLg:RegelUppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN.          
      RETURN.
   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridpaslag_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().     
      IF THIS-OBJECT:ControlWinLg:PaslagUppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN. 
         
      RETURN.
   END METHOD.
   
   
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridresursniva_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().     
      IF THIS-OBJECT:ControlWinLg:ResursNivaUppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN.         
      RETURN.
   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridresurspaslag_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().     
      IF THIS-OBJECT:ControlWinLg:ResursPaslagUppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN.          
      RETURN.
   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridresurspris_AfterCellUpdate( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinGrid.CellEventArgs ):
      DEFINE VARIABLE kolnamn  AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kolvarde AS CHARACTER NO-UNDO.
      kolnamn = e:Cell:Column:ToString().        
      kolvarde = e:Cell:VALUE:ToString().     
      IF THIS-OBJECT:ControlWinLg:ResursprisUppdate(INPUT kolnamn, INPUT kolvarde) = TRUE THEN.          
      RETURN.
   END METHOD.
   

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridPaslag_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      {AFTERDROPDEF.I}
      IF ddArgs:Grid = THIS-OBJECT:gridResursPaslag THEN DO:
         THIS-OBJECT:buttonOverResurspaslagBort_Click(?,?).
      END.
      RETURN.

   END METHOD.
   
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID gridResursPaslag_GuruAfterDropped( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      {AFTERDROPDEF.I}
      IF ddArgs:Grid = THIS-OBJECT:gridPaslag THEN DO:
         THIS-OBJECT:buttonOverResurspaslag_Click(?,?).
      END.
      RETURN.

   END METHOD.  
  
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID laggTillNivaButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):    
      THIS-OBJECT:ControlShell:nyNivaPop:Show().    

      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID laggTillPaslagButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlShell:nyPaslagPop:Show().
      THIS-OBJECT:ControlShell:fillinPaslagBenamning:GuruText:Select().
      RETURN.

   END METHOD.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID laggTillRKButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
	   IF THIS-OBJECT:SplitContainerBas1:Panel1Collapsed = TRUE THEN DO:
   		THIS-OBJECT:SplitContainerBas1:Panel1Collapsed = FALSE.
   		THIS-OBJECT:laggTillRKButton:Text = "Dölj basresurser".
   		THIS-OBJECT:laggTillRKButton:ToolTipText = "Dölj basresurser".
      END.
      ELSE DO:
   		THIS-OBJECT:SplitContainerBas1:Panel1Collapsed = TRUE.
   		THIS-OBJECT:laggTillRKButton:Text = "Visa basresurser".
         THIS-OBJECT:laggTillRKButton:ToolTipText = "Visa basresurser".
      END.
		RETURN.

	END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID taBortNiva_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):            
      THIS-OBJECT:ControlWinLg:TaBortresursniva(INTEGER(THIS-OBJECT:GridResursniva:ActiveRow:Cells["RESURSNR"]:Text), INTEGER(THIS-OBJECT:GridResursniva:ActiveRow:Cells["NIVA"]:Text)).      
      RETURN.

   END METHOD.

   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID taBortPaslagButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:TaBortPaslag().
      RETURN.

   END METHOD.

   
     /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/
   @VisualDesigner.
   METHOD PRIVATE VOID TaBortResursprisButton_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:ControlWinLg:TaBortResurspris(INTEGER(THIS-OBJECT:GridResurspris:ActiveRow:Cells["RESURSNR"]:Text)).            
      RETURN.

   END METHOD.
   
   
   METHOD PRIVATE VOID InitializeComponent ( ):
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Modules.EkgData.EkgRPControl").
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
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance49 AS Infragistics.Win.Appearance NO-UNDO.
      appearance49 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance50 AS Infragistics.Win.Appearance NO-UNDO.
      appearance50 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance51 AS Infragistics.Win.Appearance NO-UNDO.
      appearance51 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance52 AS Infragistics.Win.Appearance NO-UNDO.
      appearance52 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance53 AS Infragistics.Win.Appearance NO-UNDO.
      appearance53 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance54 AS Infragistics.Win.Appearance NO-UNDO.
      appearance54 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance55 AS Infragistics.Win.Appearance NO-UNDO.
      appearance55 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance56 AS Infragistics.Win.Appearance NO-UNDO.
      appearance56 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance57 AS Infragistics.Win.Appearance NO-UNDO.
      appearance57 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance58 AS Infragistics.Win.Appearance NO-UNDO.
      appearance58 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance59 AS Infragistics.Win.Appearance NO-UNDO.
      appearance59 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance60 AS Infragistics.Win.Appearance NO-UNDO.
      appearance60 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance61 AS Infragistics.Win.Appearance NO-UNDO.
      appearance61 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance62 AS Infragistics.Win.Appearance NO-UNDO.
      appearance62 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance63 AS Infragistics.Win.Appearance NO-UNDO.
      appearance63 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance64 AS Infragistics.Win.Appearance NO-UNDO.
      appearance64 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance65 AS Infragistics.Win.Appearance NO-UNDO.
      appearance65 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance66 AS Infragistics.Win.Appearance NO-UNDO.
      appearance66 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance67 AS Infragistics.Win.Appearance NO-UNDO.
      appearance67 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance68 AS Infragistics.Win.Appearance NO-UNDO.
      appearance68 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance69 AS Infragistics.Win.Appearance NO-UNDO.
      appearance69 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance70 AS Infragistics.Win.Appearance NO-UNDO.
      appearance70 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance71 AS Infragistics.Win.Appearance NO-UNDO.
      appearance71 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance72 AS Infragistics.Win.Appearance NO-UNDO.
      appearance72 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance73 AS Infragistics.Win.Appearance NO-UNDO.
      appearance73 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance74 AS Infragistics.Win.Appearance NO-UNDO.
      appearance74 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance75 AS Infragistics.Win.Appearance NO-UNDO.
      appearance75 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance76 AS Infragistics.Win.Appearance NO-UNDO.
      appearance76 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab23 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab23 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance77 AS Infragistics.Win.Appearance NO-UNDO.
      appearance77 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab18 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab18 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance78 AS Infragistics.Win.Appearance NO-UNDO.
      appearance78 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab21 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab21 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance79 AS Infragistics.Win.Appearance NO-UNDO.
      appearance79 = NEW Infragistics.Win.Appearance().
      THIS-OBJECT:TabRPNiva = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerNiva = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:toolStripNiva = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:laggTillNivaButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:taBortNiva = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:gridResursniva = NEW Controls.Grid().
      THIS-OBJECT:TabRPRegel = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:regelMenuSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:regeltoolStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:gridRegel = NEW Controls.Grid().
      THIS-OBJECT:TabRPPaslag = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:splitContainerPaslag = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerPaslagOver = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:paslagMenuSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:paslagToolStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:gridPaslag = NEW Controls.Grid().
      THIS-OBJECT:buttonOverResurspaslagBort = NEW Controls.ButtonOver().
      THIS-OBJECT:buttonOverResurspaslag = NEW Controls.ButtonOver().
      THIS-OBJECT:resurspaslagMenuSC = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:resursPaslagtoolStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:gridResursPaslag = NEW Controls.Grid().
      THIS-OBJECT:splitContainerBasresurser = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:toolStripBasresurser = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:gridResursForResurspris = NEW Controls.Grid().
      THIS-OBJECT:splitContainerResursPris = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerBas1 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:splitContainerBas2 = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:buttonOverResursPrisBort = NEW Controls.ButtonOver().
      THIS-OBJECT:buttonOverResurspris = NEW Controls.ButtonOver().
      THIS-OBJECT:splitContainerResursPrisLista = NEW System.Windows.Forms.SplitContainer().
      THIS-OBJECT:resurspristoolStrip = NEW System.Windows.Forms.ToolStrip().
      THIS-OBJECT:laggTillRKButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:taBortResursPrisButton = NEW System.Windows.Forms.ToolStripButton().
      THIS-OBJECT:gridResurspris = NEW Controls.Grid().
      THIS-OBJECT:TabManagerResurspriser = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:ultraTabSharedControlsPagePaslagNiv = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:TabRPNiva:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerNiva, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerNiva:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerNiva:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerNiva:SuspendLayout().
      THIS-OBJECT:toolStripNiva:SuspendLayout().
      CAST(THIS-OBJECT:gridResursniva, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabRPRegel:SuspendLayout().
      CAST(THIS-OBJECT:regelMenuSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:regelMenuSC:Panel1:SuspendLayout().
      THIS-OBJECT:regelMenuSC:Panel2:SuspendLayout().
      THIS-OBJECT:regelMenuSC:SuspendLayout().
      CAST(THIS-OBJECT:gridRegel, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabRPPaslag:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerPaslag, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerPaslag:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerPaslag:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerPaslag:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerPaslagOver, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerPaslagOver:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerPaslagOver:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerPaslagOver:SuspendLayout().
      CAST(THIS-OBJECT:paslagMenuSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:paslagMenuSC:Panel1:SuspendLayout().
      THIS-OBJECT:paslagMenuSC:Panel2:SuspendLayout().
      THIS-OBJECT:paslagMenuSC:SuspendLayout().
      CAST(THIS-OBJECT:gridPaslag, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:resurspaslagMenuSC, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:resurspaslagMenuSC:Panel1:SuspendLayout().
      THIS-OBJECT:resurspaslagMenuSC:Panel2:SuspendLayout().
      THIS-OBJECT:resurspaslagMenuSC:SuspendLayout().
      CAST(THIS-OBJECT:gridResursPaslag, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerBasresurser, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerBasresurser:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerBasresurser:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerBasresurser:SuspendLayout().
      CAST(THIS-OBJECT:gridResursForResurspris, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:splitContainerResursPris, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerResursPris:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerResursPris:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerResursPris:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerBas1, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerBas1:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerBas1:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerBas1:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerBas2, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerBas2:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerBas2:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerBas2:SuspendLayout().
      CAST(THIS-OBJECT:splitContainerResursPrisLista, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:splitContainerResursPrisLista:Panel1:SuspendLayout().
      THIS-OBJECT:splitContainerResursPrisLista:Panel2:SuspendLayout().
      THIS-OBJECT:splitContainerResursPrisLista:SuspendLayout().
      THIS-OBJECT:resurspristoolStrip:SuspendLayout().
      CAST(THIS-OBJECT:gridResurspris, System.ComponentModel.ISupportInitialize):BeginInit().
      CAST(THIS-OBJECT:TabManagerResurspriser, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabManagerResurspriser:SuspendLayout().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* TabRPNiva */
      /*  */
      THIS-OBJECT:TabRPNiva:Controls:Add(THIS-OBJECT:splitContainerNiva).
      THIS-OBJECT:TabRPNiva:Location = NEW System.Drawing.Point(1, 32).
      THIS-OBJECT:TabRPNiva:Name = "TabRPNiva".
      THIS-OBJECT:TabRPNiva:Size = NEW System.Drawing.Size(462, 518).
      /*  */
      /* splitContainerNiva */
      /*  */
      THIS-OBJECT:splitContainerNiva:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerNiva:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerNiva:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerNiva:Name = "splitContainerNiva".
      THIS-OBJECT:splitContainerNiva:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerNiva.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerNiva:Panel1:Controls:Add(THIS-OBJECT:toolStripNiva).
      /*  */
      /* splitContainerNiva.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerNiva:Panel2:Controls:Add(THIS-OBJECT:gridResursniva).
      THIS-OBJECT:splitContainerNiva:Size = NEW System.Drawing.Size(462, 518).
      THIS-OBJECT:splitContainerNiva:SplitterDistance = 25.
      THIS-OBJECT:splitContainerNiva:TabIndex = 0.
      /*  */
      /* toolStripNiva */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar0[1] = THIS-OBJECT:laggTillNivaButton.
      arrayvar0[2] = THIS-OBJECT:taBortNiva.
      THIS-OBJECT:toolStripNiva:Items:AddRange(arrayvar0).
      THIS-OBJECT:toolStripNiva:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:toolStripNiva:Name = "toolStripNiva".
      THIS-OBJECT:toolStripNiva:Size = NEW System.Drawing.Size(462, 25).
      THIS-OBJECT:toolStripNiva:TabIndex = 0.
      THIS-OBJECT:toolStripNiva:Text = "toolStrip1".
      
      
      /*  */
      /* laggTillNivaButton */
      /*  */
      THIS-OBJECT:laggTillNivaButton:Image = CAST(resources:GetObject("laggTillNivaButton.Image"), System.Drawing.Image).
      THIS-OBJECT:laggTillNivaButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:laggTillNivaButton:Name = "laggTillNivaButton".
      THIS-OBJECT:laggTillNivaButton:Size = NEW System.Drawing.Size(110, 22).
      THIS-OBJECT:laggTillNivaButton:Text = "Lägg till ny nivå".
      THIS-OBJECT:laggTillNivaButton:ToolTipText = "Lägg till resursen på ny nivå".
      THIS-OBJECT:laggTillNivaButton:Click:Subscribe(THIS-OBJECT:laggTillNivaButton_Click).
      /*  */
      /* taBortNiva */
      /*  */
      THIS-OBJECT:taBortNiva:Image = CAST(resources:GetObject("taBortNiva.Image"), System.Drawing.Image).
      THIS-OBJECT:taBortNiva:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:taBortNiva:Name = "taBortNiva".
      THIS-OBJECT:taBortNiva:Size = NEW System.Drawing.Size(88, 22).
      THIS-OBJECT:taBortNiva:Text = "Ta bort nivå".
      THIS-OBJECT:taBortNiva:ToolTipText = "Ta bort resursen för nivå".
      THIS-OBJECT:taBortNiva:Click:Subscribe(THIS-OBJECT:taBortNiva_Click).
      /*  */
      /* gridResursniva */
      /*  */
      appearance1:BackColor = System.Drawing.SystemColors:Window.
      appearance1:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridResursniva:DisplayLayout:Appearance = appearance1.
      THIS-OBJECT:gridResursniva:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridResursniva:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance2:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance2:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance2:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance2:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursniva:DisplayLayout:GroupByBox:Appearance = appearance2.
      appearance3:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursniva:DisplayLayout:GroupByBox:BandLabelAppearance = appearance3.
      THIS-OBJECT:gridResursniva:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance4:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance4:BackColor2 = System.Drawing.SystemColors:Control.
      appearance4:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance4:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursniva:DisplayLayout:GroupByBox:PromptAppearance = appearance4.
      THIS-OBJECT:gridResursniva:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridResursniva:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance5:BackColor = System.Drawing.SystemColors:Window.
      appearance5:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:ActiveCellAppearance = appearance5.
      appearance6:BackColor = System.Drawing.SystemColors:Highlight.
      appearance6:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:ActiveRowAppearance = appearance6.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance7:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:CardAreaAppearance = appearance7.
      appearance8:BorderColor = System.Drawing.Color:Silver.
      appearance8:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:CellAppearance = appearance8.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:CellPadding = 0.
      appearance9:BackColor = System.Drawing.SystemColors:Control.
      appearance9:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance9:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance9:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance9:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:GroupByRowAppearance = appearance9.
      appearance10:TextHAlignAsString = "Left".
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:HeaderAppearance = appearance10.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance11:BackColor = System.Drawing.SystemColors:Window.
      appearance11:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:RowAppearance = appearance11.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance12:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridResursniva:DisplayLayout:Override:TemplateAddRowAppearance = appearance12.
      THIS-OBJECT:gridResursniva:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridResursniva:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridResursniva:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridResursniva:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridResursniva:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridResursniva:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridResursniva:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridResursniva:GuruContext = ?.
      THIS-OBJECT:gridResursniva:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridResursniva:Name = "gridResursniva".
      THIS-OBJECT:gridResursniva:Size = NEW System.Drawing.Size(462, 489).
      THIS-OBJECT:gridResursniva:TabIndex = 2.
      THIS-OBJECT:gridResursniva:Text = "grid1".
      THIS-OBJECT:gridResursniva:AfterCellUpdate:Subscribe(THIS-OBJECT:gridresursniva_AfterCellUpdate).
      /*  */
      /* TabRPRegel */
      /*  */
      THIS-OBJECT:TabRPRegel:Controls:Add(THIS-OBJECT:regelMenuSC).
      THIS-OBJECT:TabRPRegel:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabRPRegel:Name = "TabRPRegel".
      THIS-OBJECT:TabRPRegel:Size = NEW System.Drawing.Size(462, 518).
      /*  */
      /* regelMenuSC */
      /*  */
      THIS-OBJECT:regelMenuSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:regelMenuSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:regelMenuSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:regelMenuSC:Name = "regelMenuSC".
      THIS-OBJECT:regelMenuSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* regelMenuSC.Panel1 */
      /*  */
      THIS-OBJECT:regelMenuSC:Panel1:Controls:Add(THIS-OBJECT:regeltoolStrip).
      /*  */
      /* regelMenuSC.Panel2 */
      /*  */
      THIS-OBJECT:regelMenuSC:Panel2:Controls:Add(THIS-OBJECT:gridRegel).
      THIS-OBJECT:regelMenuSC:Size = NEW System.Drawing.Size(462, 518).
      THIS-OBJECT:regelMenuSC:SplitterDistance = 25.
      THIS-OBJECT:regelMenuSC:TabIndex = 1.
      /*  */
      /* regeltoolStrip */
      /*  */
      THIS-OBJECT:regeltoolStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:regeltoolStrip:Name = "regeltoolStrip".
      THIS-OBJECT:regeltoolStrip:Size = NEW System.Drawing.Size(462, 25).
      THIS-OBJECT:regeltoolStrip:TabIndex = 0.
      THIS-OBJECT:regeltoolStrip:Text = "toolStrip2".
      /*  */
      /* gridRegel */
      /*  */
      appearance13:BackColor = System.Drawing.SystemColors:Window.
      appearance13:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridRegel:DisplayLayout:Appearance = appearance13.
      THIS-OBJECT:gridRegel:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridRegel:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance14:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance14:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance14:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance14:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridRegel:DisplayLayout:GroupByBox:Appearance = appearance14.
      appearance15:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridRegel:DisplayLayout:GroupByBox:BandLabelAppearance = appearance15.
      THIS-OBJECT:gridRegel:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance16:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance16:BackColor2 = System.Drawing.SystemColors:Control.
      appearance16:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance16:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridRegel:DisplayLayout:GroupByBox:PromptAppearance = appearance16.
      THIS-OBJECT:gridRegel:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridRegel:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance17:BackColor = System.Drawing.SystemColors:Window.
      appearance17:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:ActiveCellAppearance = appearance17.
      appearance18:BackColor = System.Drawing.SystemColors:Highlight.
      appearance18:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:ActiveRowAppearance = appearance18.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance19:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:CardAreaAppearance = appearance19.
      appearance20:BorderColor = System.Drawing.Color:Silver.
      appearance20:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:CellAppearance = appearance20.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:CellPadding = 0.
      appearance21:BackColor = System.Drawing.SystemColors:Control.
      appearance21:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance21:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance21:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance21:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:GroupByRowAppearance = appearance21.
      appearance22:TextHAlignAsString = "Left".
      THIS-OBJECT:gridRegel:DisplayLayout:Override:HeaderAppearance = appearance22.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance23:BackColor = System.Drawing.SystemColors:Window.
      appearance23:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:RowAppearance = appearance23.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance24:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridRegel:DisplayLayout:Override:TemplateAddRowAppearance = appearance24.
      THIS-OBJECT:gridRegel:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridRegel:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridRegel:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridRegel:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridRegel:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridRegel:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridRegel:GuruContext = ?.
      THIS-OBJECT:gridRegel:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridRegel:Name = "gridRegel".
      THIS-OBJECT:gridRegel:Size = NEW System.Drawing.Size(787, 498).
      THIS-OBJECT:gridRegel:TabIndex = 3.
      THIS-OBJECT:gridRegel:Text = "grid2".
      THIS-OBJECT:gridRegel:AfterCellUpdate:Subscribe(THIS-OBJECT:gridregel_AfterCellUpdate).
      /*  */
      /* TabRPPaslag */
      /*  */
      THIS-OBJECT:TabRPPaslag:Controls:Add(THIS-OBJECT:splitContainerPaslag).
      THIS-OBJECT:TabRPPaslag:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:TabRPPaslag:Name = "TabRPPaslag".
      THIS-OBJECT:TabRPPaslag:Size = NEW System.Drawing.Size(462, 518).
      /*  */
      /* splitContainerPaslag */
      /*  */
      THIS-OBJECT:splitContainerPaslag:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerPaslag:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerPaslag:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerPaslag:Name = "splitContainerPaslag".
      /*  */
      /* splitContainerPaslag.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerPaslag:Panel1:Controls:Add(THIS-OBJECT:splitContainerPaslagOver).
      /*  */
      /* splitContainerPaslag.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerPaslag:Panel2:Controls:Add(THIS-OBJECT:resurspaslagMenuSC).
      THIS-OBJECT:splitContainerPaslag:Size = NEW System.Drawing.Size(462, 518).
      THIS-OBJECT:splitContainerPaslag:SplitterDistance = 362.
      THIS-OBJECT:splitContainerPaslag:TabIndex = 0.
      /*  */
      /* splitContainerPaslagOver */
      /*  */
      THIS-OBJECT:splitContainerPaslagOver:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerPaslagOver:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerPaslagOver:Name = "splitContainerPaslagOver".
      /*  */
      /* splitContainerPaslagOver.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerPaslagOver:Panel1:Controls:Add(THIS-OBJECT:paslagMenuSC).
      /*  */
      /* splitContainerPaslagOver.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerPaslagOver:Panel2:Controls:Add(THIS-OBJECT:buttonOverResurspaslagBort).
      THIS-OBJECT:splitContainerPaslagOver:Panel2:Controls:Add(THIS-OBJECT:buttonOverResurspaslag).
      THIS-OBJECT:splitContainerPaslagOver:Size = NEW System.Drawing.Size(362, 518).
      THIS-OBJECT:splitContainerPaslagOver:SplitterDistance = 328.
      THIS-OBJECT:splitContainerPaslagOver:TabIndex = 0.
      /*  */
      /* paslagMenuSC */
      /*  */
      THIS-OBJECT:paslagMenuSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:paslagMenuSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:paslagMenuSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:paslagMenuSC:Name = "paslagMenuSC".
      THIS-OBJECT:paslagMenuSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* paslagMenuSC.Panel1 */
      /*  */
      THIS-OBJECT:paslagMenuSC:Panel1:Controls:Add(THIS-OBJECT:paslagToolStrip).
      /*  */
      /* paslagMenuSC.Panel2 */
      /*  */
      THIS-OBJECT:paslagMenuSC:Panel2:Controls:Add(THIS-OBJECT:gridPaslag).
      THIS-OBJECT:paslagMenuSC:Size = NEW System.Drawing.Size(328, 518).
      THIS-OBJECT:paslagMenuSC:SplitterDistance = 25.
      THIS-OBJECT:paslagMenuSC:TabIndex = 1.
      /*  */
      /* paslagToolStrip */
      /*  */
      THIS-OBJECT:paslagToolStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:paslagToolStrip:Name = "paslagToolStrip".
      THIS-OBJECT:paslagToolStrip:Size = NEW System.Drawing.Size(328, 25).
      THIS-OBJECT:paslagToolStrip:TabIndex = 1.
      THIS-OBJECT:paslagToolStrip:Text = "toolStrip1".
      /*  */
      /* gridPaslag */
      /*  */
      THIS-OBJECT:gridPaslag:AllowDrop = TRUE.
      appearance25:BackColor = System.Drawing.SystemColors:Window.
      appearance25:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridPaslag:DisplayLayout:Appearance = appearance25.
      THIS-OBJECT:gridPaslag:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridPaslag:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance26:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance26:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance26:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance26:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridPaslag:DisplayLayout:GroupByBox:Appearance = appearance26.
      appearance27:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridPaslag:DisplayLayout:GroupByBox:BandLabelAppearance = appearance27.
      THIS-OBJECT:gridPaslag:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance28:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance28:BackColor2 = System.Drawing.SystemColors:Control.
      appearance28:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance28:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridPaslag:DisplayLayout:GroupByBox:PromptAppearance = appearance28.
      THIS-OBJECT:gridPaslag:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridPaslag:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance29:BackColor = System.Drawing.SystemColors:Window.
      appearance29:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:ActiveCellAppearance = appearance29.
      appearance30:BackColor = System.Drawing.SystemColors:Highlight.
      appearance30:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:ActiveRowAppearance = appearance30.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance31:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:CardAreaAppearance = appearance31.
      appearance32:BorderColor = System.Drawing.Color:Silver.
      appearance32:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:CellAppearance = appearance32.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:CellPadding = 0.
      appearance33:BackColor = System.Drawing.SystemColors:Control.
      appearance33:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance33:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance33:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance33:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:GroupByRowAppearance = appearance33.
      appearance34:TextHAlignAsString = "Left".
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:HeaderAppearance = appearance34.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance35:BackColor = System.Drawing.SystemColors:Window.
      appearance35:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:RowAppearance = appearance35.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance36:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridPaslag:DisplayLayout:Override:TemplateAddRowAppearance = appearance36.
      THIS-OBJECT:gridPaslag:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridPaslag:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridPaslag:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridPaslag:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridPaslag:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridPaslag:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridPaslag:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridPaslag:GuruContext = ?.
      THIS-OBJECT:gridPaslag:GuruDraggable = TRUE.
      THIS-OBJECT:gridPaslag:GuruDroppable = TRUE.
      THIS-OBJECT:gridPaslag:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridPaslag:Name = "gridPaslag".
      THIS-OBJECT:gridPaslag:Size = NEW System.Drawing.Size(328, 489).
      THIS-OBJECT:gridPaslag:TabIndex = 0.
      THIS-OBJECT:gridPaslag:Text = "grid1".
      THIS-OBJECT:gridPaslag:GuruAfterDropped:Subscribe(THIS-OBJECT:gridPaslag_GuruAfterDropped).
      THIS-OBJECT:gridPaslag:AfterCellUpdate:Subscribe(THIS-OBJECT:gridpaslag_AfterCellUpdate).
      /*  */
      /* buttonOverResurspaslagBort */
      /*  */
      appearance37:Image = CAST(resources:GetObject("appearance37.Image"), System.Object).
      THIS-OBJECT:buttonOverResurspaslagBort:Appearance = appearance37.
      THIS-OBJECT:buttonOverResurspaslagBort:Location = NEW System.Drawing.Point(3, 50).
      THIS-OBJECT:buttonOverResurspaslagBort:Name = "buttonOverResurspaslagBort".
      THIS-OBJECT:buttonOverResurspaslagBort:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverResurspaslagBort:TabIndex = 7.
      THIS-OBJECT:buttonOverResurspaslagBort:Click:Subscribe(THIS-OBJECT:buttonOverResurspaslagBort_Click).
      /*  */
      /* buttonOverResurspaslag */
      /*  */
      appearance38:Image = CAST(resources:GetObject("appearance38.Image"), System.Object).
      THIS-OBJECT:buttonOverResurspaslag:Appearance = appearance38.
      THIS-OBJECT:buttonOverResurspaslag:Location = NEW System.Drawing.Point(3, 5).
      THIS-OBJECT:buttonOverResurspaslag:Name = "buttonOverResurspaslag".
      THIS-OBJECT:buttonOverResurspaslag:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverResurspaslag:TabIndex = 6.
      THIS-OBJECT:buttonOverResurspaslag:Click:Subscribe(THIS-OBJECT:buttonOverResurspaslag_Click).
      /*  */
      /* resurspaslagMenuSC */
      /*  */
      THIS-OBJECT:resurspaslagMenuSC:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:resurspaslagMenuSC:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:resurspaslagMenuSC:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:resurspaslagMenuSC:Name = "resurspaslagMenuSC".
      THIS-OBJECT:resurspaslagMenuSC:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* resurspaslagMenuSC.Panel1 */
      /*  */
      THIS-OBJECT:resurspaslagMenuSC:Panel1:Controls:Add(THIS-OBJECT:resursPaslagtoolStrip).
      /*  */
      /* resurspaslagMenuSC.Panel2 */
      /*  */
      THIS-OBJECT:resurspaslagMenuSC:Panel2:Controls:Add(THIS-OBJECT:gridResursPaslag).
      THIS-OBJECT:resurspaslagMenuSC:Size = NEW System.Drawing.Size(96, 518).
      THIS-OBJECT:resurspaslagMenuSC:SplitterDistance = 25.
      THIS-OBJECT:resurspaslagMenuSC:TabIndex = 2.
      /*  */
      /* resursPaslagtoolStrip */
      /*  */
      THIS-OBJECT:resursPaslagtoolStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:resursPaslagtoolStrip:Name = "resursPaslagtoolStrip".
      THIS-OBJECT:resursPaslagtoolStrip:Size = NEW System.Drawing.Size(96, 25).
      THIS-OBJECT:resursPaslagtoolStrip:TabIndex = 1.
      THIS-OBJECT:resursPaslagtoolStrip:Text = "toolStrip1".
      /*  */
      /* gridResursPaslag */
      /*  */
      THIS-OBJECT:gridResursPaslag:AllowDrop = TRUE.
      appearance39:BackColor = System.Drawing.SystemColors:Window.
      appearance39:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Appearance = appearance39.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance40:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance40:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance40:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance40:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:GroupByBox:Appearance = appearance40.
      appearance41:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:GroupByBox:BandLabelAppearance = appearance41.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance42:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance42:BackColor2 = System.Drawing.SystemColors:Control.
      appearance42:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance42:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:GroupByBox:PromptAppearance = appearance42.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance43:BackColor = System.Drawing.SystemColors:Window.
      appearance43:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:ActiveCellAppearance = appearance43.
      appearance44:BackColor = System.Drawing.SystemColors:Highlight.
      appearance44:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:ActiveRowAppearance = appearance44.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance45:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:CardAreaAppearance = appearance45.
      appearance46:BorderColor = System.Drawing.Color:Silver.
      appearance46:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:CellAppearance = appearance46.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:CellPadding = 0.
      appearance47:BackColor = System.Drawing.SystemColors:Control.
      appearance47:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance47:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance47:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance47:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:GroupByRowAppearance = appearance47.
      appearance48:TextHAlignAsString = "Left".
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:HeaderAppearance = appearance48.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance49:BackColor = System.Drawing.SystemColors:Window.
      appearance49:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:RowAppearance = appearance49.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance50:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:Override:TemplateAddRowAppearance = appearance50.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridResursPaslag:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridResursPaslag:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridResursPaslag:GuruContext = ?.
      THIS-OBJECT:gridResursPaslag:GuruDraggable = TRUE.
      THIS-OBJECT:gridResursPaslag:GuruDroppable = TRUE.
      THIS-OBJECT:gridResursPaslag:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridResursPaslag:Name = "gridResursPaslag".
      THIS-OBJECT:gridResursPaslag:Size = NEW System.Drawing.Size(96, 489).
      THIS-OBJECT:gridResursPaslag:TabIndex = 0.
      THIS-OBJECT:gridResursPaslag:Text = "grid1".
      THIS-OBJECT:gridResursPaslag:GuruAfterDropped:Subscribe(THIS-OBJECT:gridResursPaslag_GuruAfterDropped).
      THIS-OBJECT:gridResursPaslag:AfterCellUpdate:Subscribe(THIS-OBJECT:gridresurspaslag_AfterCellUpdate).
      /*  */
      /* splitContainerBasresurser */
      /*  */
      THIS-OBJECT:splitContainerBasresurser:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerBasresurser:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerBasresurser:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerBasresurser:Name = "splitContainerBasresurser".
      THIS-OBJECT:splitContainerBasresurser:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerBasresurser.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerBasresurser:Panel1:Controls:Add(THIS-OBJECT:toolStripBasresurser).
      /*  */
      /* splitContainerBasresurser.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerBasresurser:Panel2:Controls:Add(THIS-OBJECT:gridResursForResurspris).
      THIS-OBJECT:splitContainerBasresurser:Size = NEW System.Drawing.Size(316, 553).
      THIS-OBJECT:splitContainerBasresurser:SplitterDistance = 25.
      THIS-OBJECT:splitContainerBasresurser:TabIndex = 0.
      /*  */
      /* toolStripBasresurser */
      /*  */
      THIS-OBJECT:toolStripBasresurser:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:toolStripBasresurser:Name = "toolStripBasresurser".
      THIS-OBJECT:toolStripBasresurser:Size = NEW System.Drawing.Size(316, 25).
      THIS-OBJECT:toolStripBasresurser:TabIndex = 0.
      THIS-OBJECT:toolStripBasresurser:Text = "toolStrip2".
      /*  */
      /* gridResursForResurspris */
      /*  */
      appearance51:BackColor = System.Drawing.SystemColors:Window.
      appearance51:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Appearance = appearance51.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance52:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance52:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance52:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance52:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:GroupByBox:Appearance = appearance52.
      appearance53:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:GroupByBox:BandLabelAppearance = appearance53.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance54:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance54:BackColor2 = System.Drawing.SystemColors:Control.
      appearance54:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance54:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:GroupByBox:PromptAppearance = appearance54.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance55:BackColor = System.Drawing.SystemColors:Window.
      appearance55:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:ActiveCellAppearance = appearance55.
      appearance56:BackColor = System.Drawing.SystemColors:Highlight.
      appearance56:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:ActiveRowAppearance = appearance56.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance57:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:CardAreaAppearance = appearance57.
      appearance58:BorderColor = System.Drawing.Color:Silver.
      appearance58:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:CellAppearance = appearance58.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:CellPadding = 0.
      appearance59:BackColor = System.Drawing.SystemColors:Control.
      appearance59:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance59:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance59:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance59:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:GroupByRowAppearance = appearance59.
      appearance60:TextHAlignAsString = "Left".
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:HeaderAppearance = appearance60.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance61:BackColor = System.Drawing.SystemColors:Window.
      appearance61:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:RowAppearance = appearance61.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance62:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:Override:TemplateAddRowAppearance = appearance62.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridResursForResurspris:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridResursForResurspris:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridResursForResurspris:GuruContext = ?.
      THIS-OBJECT:gridResursForResurspris:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridResursForResurspris:Name = "gridResursForResurspris".
      THIS-OBJECT:gridResursForResurspris:Size = NEW System.Drawing.Size(316, 524).
      THIS-OBJECT:gridResursForResurspris:TabIndex = 1.
      THIS-OBJECT:gridResursForResurspris:Text = "grid1".
      /*  */
      /* splitContainerResursPris */
      /*  */
      THIS-OBJECT:splitContainerResursPris:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerResursPris:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerResursPris:Name = "splitContainerResursPris".
      /*  */
      /* splitContainerResursPris.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerResursPris:Panel1:Controls:Add(THIS-OBJECT:splitContainerBas1).
      /*  */
      /* splitContainerResursPris.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerResursPris:Panel2:Controls:Add(THIS-OBJECT:TabManagerResurspriser).
      THIS-OBJECT:splitContainerResursPris:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:splitContainerResursPris:SplitterDistance = 675.
      THIS-OBJECT:splitContainerResursPris:TabIndex = 5.
      /*  */
      /* splitContainerBas1 */
      /*  */
      THIS-OBJECT:splitContainerBas1:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerBas1:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerBas1:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerBas1:Name = "splitContainerBas1".
      /*  */
      /* splitContainerBas1.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerBas1:Panel1:Controls:Add(THIS-OBJECT:splitContainerBas2).
      /*  */
      /* splitContainerBas1.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerBas1:Panel2:Controls:Add(THIS-OBJECT:splitContainerResursPrisLista).
      THIS-OBJECT:splitContainerBas1:Size = NEW System.Drawing.Size(675, 553).
      THIS-OBJECT:splitContainerBas1:SplitterDistance = 350.
      THIS-OBJECT:splitContainerBas1:TabIndex = 1.
      /*  */
      /* splitContainerBas2 */
      /*  */
      THIS-OBJECT:splitContainerBas2:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerBas2:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerBas2:Name = "splitContainerBas2".
      /*  */
      /* splitContainerBas2.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerBas2:Panel1:Controls:Add(THIS-OBJECT:splitContainerBasresurser).
      /*  */
      /* splitContainerBas2.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerBas2:Panel2:Controls:Add(THIS-OBJECT:buttonOverResursPrisBort).
      THIS-OBJECT:splitContainerBas2:Panel2:Controls:Add(THIS-OBJECT:buttonOverResurspris).
      THIS-OBJECT:splitContainerBas2:Size = NEW System.Drawing.Size(350, 553).
      THIS-OBJECT:splitContainerBas2:SplitterDistance = 316.
      THIS-OBJECT:splitContainerBas2:TabIndex = 0.
      /*  */
      /* buttonOverResursPrisBort */
      /*  */
      appearance63:Image = CAST(resources:GetObject("appearance63.Image"), System.Object).
      THIS-OBJECT:buttonOverResursPrisBort:Appearance = appearance63.
      THIS-OBJECT:buttonOverResursPrisBort:Location = NEW System.Drawing.Point(5, 160).
      THIS-OBJECT:buttonOverResursPrisBort:Name = "buttonOverResursPrisBort".
      THIS-OBJECT:buttonOverResursPrisBort:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverResursPrisBort:TabIndex = 15.
      THIS-OBJECT:buttonOverResursPrisBort:Text = "buttonOver1".
      THIS-OBJECT:buttonOverResursPrisBort:Click:Subscribe(THIS-OBJECT:buttonOverResursPrisBort_Click).
      /*  */
      /* buttonOverResurspris */
      /*  */
      appearance64:Image = CAST(resources:GetObject("appearance64.Image"), System.Object).
      THIS-OBJECT:buttonOverResurspris:Appearance = appearance64.
      THIS-OBJECT:buttonOverResurspris:Location = NEW System.Drawing.Point(5, 115).
      THIS-OBJECT:buttonOverResurspris:Name = "buttonOverResurspris".
      THIS-OBJECT:buttonOverResurspris:Size = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:buttonOverResurspris:TabIndex = 14.
      THIS-OBJECT:buttonOverResurspris:Text = "buttonOver1".
      THIS-OBJECT:buttonOverResurspris:Click:Subscribe(THIS-OBJECT:buttonOverResurspris_Click).
      /*  */
      /* splitContainerResursPrisLista */
      /*  */
      THIS-OBJECT:splitContainerResursPrisLista:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:splitContainerResursPrisLista:FixedPanel = System.Windows.Forms.FixedPanel:Panel1.
      THIS-OBJECT:splitContainerResursPrisLista:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:splitContainerResursPrisLista:Name = "splitContainerResursPrisLista".
      THIS-OBJECT:splitContainerResursPrisLista:Orientation = System.Windows.Forms.Orientation:Horizontal.
      /*  */
      /* splitContainerResursPrisLista.Panel1 */
      /*  */
      THIS-OBJECT:splitContainerResursPrisLista:Panel1:Controls:Add(THIS-OBJECT:resurspristoolStrip).
      /*  */
      /* splitContainerResursPrisLista.Panel2 */
      /*  */
      THIS-OBJECT:splitContainerResursPrisLista:Panel2:Controls:Add(THIS-OBJECT:gridResurspris).
      THIS-OBJECT:splitContainerResursPrisLista:Size = NEW System.Drawing.Size(321, 553).
      THIS-OBJECT:splitContainerResursPrisLista:SplitterDistance = 25.
      THIS-OBJECT:splitContainerResursPrisLista:TabIndex = 0.
      /*  */
      /* resurspristoolStrip */
      /*  */
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS System.Windows.Forms.ToolStripItem EXTENT 2 NO-UNDO.
      arrayvar1[1] = THIS-OBJECT:laggTillRKButton.
      arrayvar1[2] = THIS-OBJECT:taBortResursPrisButton.
      THIS-OBJECT:resurspristoolStrip:Items:AddRange(arrayvar1).
      THIS-OBJECT:resurspristoolStrip:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:resurspristoolStrip:Name = "resurspristoolStrip".
      THIS-OBJECT:resurspristoolStrip:Size = NEW System.Drawing.Size(321, 25).
      THIS-OBJECT:resurspristoolStrip:TabIndex = 0.
      THIS-OBJECT:resurspristoolStrip:Text = "toolStrip3".
      /*  */
      /* laggTillRKButton */
      /*  */
      THIS-OBJECT:laggTillRKButton:Image = CAST(resources:GetObject("laggTillRKButton.Image"), System.Drawing.Image).
      THIS-OBJECT:laggTillRKButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:laggTillRKButton:Name = "laggTillRKButton".
      THIS-OBJECT:laggTillRKButton:Size = NEW System.Drawing.Size(110, 22).
      THIS-OBJECT:laggTillRKButton:Text = "Visa basresurser".
      THIS-OBJECT:laggTillRKButton:ToolTipText = "Visa basresurser".
      THIS-OBJECT:laggTillRKButton:Click:Subscribe(THIS-OBJECT:laggTillRKButton_Click).
      /*  */
      /* taBortResursPrisButton */
      /*  */
      THIS-OBJECT:taBortResursPrisButton:Image = CAST(resources:GetObject("taBortResursPrisButton.Image"), System.Drawing.Image).
      THIS-OBJECT:taBortResursPrisButton:ImageTransparentColor = System.Drawing.Color:Magenta.
      THIS-OBJECT:taBortResursPrisButton:Name = "taBortResursPrisButton".
      THIS-OBJECT:taBortResursPrisButton:Size = NEW System.Drawing.Size(97, 22).
      THIS-OBJECT:taBortResursPrisButton:Text = "Ta bort resurs".
      THIS-OBJECT:taBortResursPrisButton:ToolTipText = "Tar bort den markerade resursen från katalogen".
      THIS-OBJECT:taBortResursPrisButton:Click:Subscribe(THIS-OBJECT:TaBortResursprisButton_Click).
      /*  */
      /* gridResurspris */
      /*  */
      appearance65:BackColor = System.Drawing.SystemColors:Window.
      appearance65:BorderColor = System.Drawing.SystemColors:InactiveCaption.
      THIS-OBJECT:gridResurspris:DisplayLayout:Appearance = appearance65.
      THIS-OBJECT:gridResurspris:DisplayLayout:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      THIS-OBJECT:gridResurspris:DisplayLayout:CaptionVisible = Infragistics.Win.DefaultableBoolean:False.
      appearance66:BackColor = System.Drawing.SystemColors:ActiveBorder.
      appearance66:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance66:BackGradientStyle = Infragistics.Win.GradientStyle:Vertical.
      appearance66:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResurspris:DisplayLayout:GroupByBox:Appearance = appearance66.
      appearance67:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResurspris:DisplayLayout:GroupByBox:BandLabelAppearance = appearance67.
      THIS-OBJECT:gridResurspris:DisplayLayout:GroupByBox:BorderStyle = Infragistics.Win.UIElementBorderStyle:Solid.
      appearance68:BackColor = System.Drawing.SystemColors:ControlLightLight.
      appearance68:BackColor2 = System.Drawing.SystemColors:Control.
      appearance68:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance68:ForeColor = System.Drawing.SystemColors:GrayText.
      THIS-OBJECT:gridResurspris:DisplayLayout:GroupByBox:PromptAppearance = appearance68.
      THIS-OBJECT:gridResurspris:DisplayLayout:MaxColScrollRegions = 1.
      THIS-OBJECT:gridResurspris:DisplayLayout:MaxRowScrollRegions = 1.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:ActiveAppearancesEnabled = Infragistics.Win.DefaultableBoolean:True.
      appearance69:BackColor = System.Drawing.SystemColors:Window.
      appearance69:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:ActiveCellAppearance = appearance69.
      appearance70:BackColor = System.Drawing.SystemColors:Highlight.
      appearance70:ForeColor = System.Drawing.SystemColors:HighlightText.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:ActiveRowAppearance = appearance70.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:BorderStyleCell = Infragistics.Win.UIElementBorderStyle:Dotted.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:BorderStyleRow = Infragistics.Win.UIElementBorderStyle:Dotted.
      appearance71:BackColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:CardAreaAppearance = appearance71.
      appearance72:BorderColor = System.Drawing.Color:Silver.
      appearance72:TextTrimming = Infragistics.Win.TextTrimming:EllipsisCharacter.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:CellAppearance = appearance72.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:CellClickAction = Infragistics.Win.UltraWinGrid.CellClickAction:EditAndSelectText.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:CellPadding = 0.
      appearance73:BackColor = System.Drawing.SystemColors:Control.
      appearance73:BackColor2 = System.Drawing.SystemColors:ControlDark.
      appearance73:BackGradientAlignment = Infragistics.Win.GradientAlignment:Element.
      appearance73:BackGradientStyle = Infragistics.Win.GradientStyle:Horizontal.
      appearance73:BorderColor = System.Drawing.SystemColors:Window.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:GroupByRowAppearance = appearance73.
      appearance74:TextHAlignAsString = "Left".
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:HeaderAppearance = appearance74.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:HeaderClickAction = Infragistics.Win.UltraWinGrid.HeaderClickAction:SortMulti.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:HeaderStyle = Infragistics.Win.HeaderStyle:WindowsXPCommand.
      appearance75:BackColor = System.Drawing.SystemColors:Window.
      appearance75:BorderColor = System.Drawing.Color:Silver.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:RowAppearance = appearance75.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:RowSelectors = Infragistics.Win.DefaultableBoolean:False.
      appearance76:BackColor = System.Drawing.SystemColors:ControlLight.
      THIS-OBJECT:gridResurspris:DisplayLayout:Override:TemplateAddRowAppearance = appearance76.
      THIS-OBJECT:gridResurspris:DisplayLayout:ScrollBounds = Infragistics.Win.UltraWinGrid.ScrollBounds:ScrollToFill.
      THIS-OBJECT:gridResurspris:DisplayLayout:ScrollStyle = Infragistics.Win.UltraWinGrid.ScrollStyle:Immediate.
      THIS-OBJECT:gridResurspris:DisplayLayout:SelectionOverlayBorderColor = System.Drawing.SystemColors:Highlight.
      THIS-OBJECT:gridResurspris:DisplayLayout:SelectionOverlayBorderThickness = 2.
      THIS-OBJECT:gridResurspris:DisplayLayout:SelectionOverlayColor = System.Drawing.SystemColors:HotTrack.
      THIS-OBJECT:gridResurspris:DisplayLayout:ViewStyleBand = Infragistics.Win.UltraWinGrid.ViewStyleBand:OutlookGroupBy.
      THIS-OBJECT:gridResurspris:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:gridResurspris:GuruContext = ?.
      THIS-OBJECT:gridResurspris:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:gridResurspris:Name = "gridResurspris".
      THIS-OBJECT:gridResurspris:Size = NEW System.Drawing.Size(321, 524).
      THIS-OBJECT:gridResurspris:TabIndex = 1.
      THIS-OBJECT:gridResurspris:Text = "grid1".
      THIS-OBJECT:gridResurspris:AfterCellUpdate:Subscribe(THIS-OBJECT:gridresurspris_AfterCellUpdate).
      /*  */
      /* TabManagerResurspriser */
      /*  */
      THIS-OBJECT:TabManagerResurspriser:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPagePaslagNiv).
      THIS-OBJECT:TabManagerResurspriser:Controls:Add(THIS-OBJECT:TabRPPaslag).
      THIS-OBJECT:TabManagerResurspriser:Controls:Add(THIS-OBJECT:TabRPNiva).
      THIS-OBJECT:TabManagerResurspriser:Controls:Add(THIS-OBJECT:TabRPRegel).
      THIS-OBJECT:TabManagerResurspriser:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:TabManagerResurspriser:ImageSize = NEW System.Drawing.Size(25, 25).
      THIS-OBJECT:TabManagerResurspriser:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:TabManagerResurspriser:Name = "TabManagerResurspriser".
      THIS-OBJECT:TabManagerResurspriser:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPagePaslagNiv.
      THIS-OBJECT:TabManagerResurspriser:Size = NEW System.Drawing.Size(466, 553).
      THIS-OBJECT:TabManagerResurspriser:TabIndex = 0.
      appearance77:Image = CAST(resources:GetObject("appearance77.Image"), System.Object).
      ultraTab23:Appearance = appearance77.
      ultraTab23:TabPage = THIS-OBJECT:TabRPNiva.
      ultraTab23:Text = "Nivåer".
      appearance78:Image = CAST(resources:GetObject("appearance78.Image"), System.Object).
      ultraTab18:Appearance = appearance78.
      ultraTab18:TabPage = THIS-OBJECT:TabRPRegel.
      ultraTab18:Text = "Regel".
      appearance79:Image = CAST(resources:GetObject("appearance79.Image"), System.Object).
      ultraTab21:Appearance = appearance79.
      ultraTab21:TabPage = THIS-OBJECT:TabRPPaslag.
      ultraTab21:Text = "Påslag".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 3 NO-UNDO.
      arrayvar2[1] = ultraTab23.
      arrayvar2[2] = ultraTab18.
      arrayvar2[3] = ultraTab21.
      THIS-OBJECT:TabManagerResurspriser:Tabs:AddRange(arrayvar2).
      /*  */
      /* ultraTabSharedControlsPagePaslagNiv */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPagePaslagNiv:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabSharedControlsPagePaslagNiv:Name = "ultraTabSharedControlsPagePaslagNiv".
      THIS-OBJECT:ultraTabSharedControlsPagePaslagNiv:Size = NEW System.Drawing.Size(462, 518).
      /*  */
      /* EkgRPControl */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:splitContainerResursPris).
      THIS-OBJECT:Name = "EkgRPControl".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1145, 553).
      THIS-OBJECT:TabRPNiva:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNiva:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerNiva:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerNiva:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerNiva, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerNiva:ResumeLayout(FALSE).
      THIS-OBJECT:toolStripNiva:ResumeLayout(FALSE).
      THIS-OBJECT:toolStripNiva:PerformLayout().
      CAST(THIS-OBJECT:gridResursniva, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabRPRegel:ResumeLayout(FALSE).
      THIS-OBJECT:regelMenuSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:regelMenuSC:Panel1:PerformLayout().
      THIS-OBJECT:regelMenuSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:regelMenuSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:regelMenuSC:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:gridRegel, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabRPPaslag:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerPaslag:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerPaslag:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerPaslag, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerPaslag:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerPaslagOver:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerPaslagOver:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerPaslagOver, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerPaslagOver:ResumeLayout(FALSE).
      THIS-OBJECT:paslagMenuSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:paslagMenuSC:Panel1:PerformLayout().
      THIS-OBJECT:paslagMenuSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:paslagMenuSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:paslagMenuSC:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:gridPaslag, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:resurspaslagMenuSC:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:resurspaslagMenuSC:Panel1:PerformLayout().
      THIS-OBJECT:resurspaslagMenuSC:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:resurspaslagMenuSC, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:resurspaslagMenuSC:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:gridResursPaslag, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerBasresurser:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerBasresurser:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerBasresurser:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerBasresurser, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerBasresurser:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:gridResursForResurspris, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerResursPris:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerResursPris:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerResursPris, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerResursPris:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerBas1:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerBas1:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerBas1, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerBas1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerBas2:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerBas2:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerBas2, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerBas2:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerResursPrisLista:Panel1:ResumeLayout(FALSE).
      THIS-OBJECT:splitContainerResursPrisLista:Panel1:PerformLayout().
      THIS-OBJECT:splitContainerResursPrisLista:Panel2:ResumeLayout(FALSE).
      CAST(THIS-OBJECT:splitContainerResursPrisLista, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:splitContainerResursPrisLista:ResumeLayout(FALSE).
      THIS-OBJECT:resurspristoolStrip:ResumeLayout(FALSE).
      THIS-OBJECT:resurspristoolStrip:PerformLayout().
      CAST(THIS-OBJECT:gridResurspris, System.ComponentModel.ISupportInitialize):EndInit().
      CAST(THIS-OBJECT:TabManagerResurspriser, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabManagerResurspriser:ResumeLayout(FALSE).
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
   END METHOD.
     
  
END CLASS.