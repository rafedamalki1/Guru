 
 /*------------------------------------------------------------------------
    File        : KabelShell
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Wed Nov 23 12:52:20 CET 2016
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.



CLASS Modules.Beredning.KabelShell INHERITS UserControl: 
	
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE Skapval AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE SkapMtrl AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE SkapKort AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE SkapAnm AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE KonsSkap AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE IdSkap AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PRIVATE VARIABLE TabKabelMan AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.

		
	CONSTRUCTOR PUBLIC KabelShell (  ):
		
		
      SUPER().
      InitializeComponent().
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
      DEFINE VARIABLE ultraTab1 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab1 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab2 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab2 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab3 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab3 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab4 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab4 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab5 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab5 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTab6 AS Infragistics.Win.UltraWinTabControl.UltraTab NO-UNDO.
      ultraTab6 = NEW Infragistics.Win.UltraWinTabControl.UltraTab().
      THIS-OBJECT:IdSkap = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:TabKabelMan = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:KonsSkap = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:Skapval = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:SkapMtrl = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:SkapKort = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      THIS-OBJECT:SkapAnm = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      CAST(THIS-OBJECT:TabKabelMan, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabKabelMan:SuspendLayout().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* IdSkap */
      /*  */
      THIS-OBJECT:IdSkap:Location = NEW System.Drawing.Point(1, 1).
      THIS-OBJECT:IdSkap:Name = "IdSkap".
      THIS-OBJECT:IdSkap:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* TabKabelMan */
      /*  */
      THIS-OBJECT:TabKabelMan:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
      THIS-OBJECT:TabKabelMan:Controls:Add(THIS-OBJECT:IdSkap).
      THIS-OBJECT:TabKabelMan:Controls:Add(THIS-OBJECT:KonsSkap).
      THIS-OBJECT:TabKabelMan:Controls:Add(THIS-OBJECT:Skapval).
      THIS-OBJECT:TabKabelMan:Controls:Add(THIS-OBJECT:SkapMtrl).
      THIS-OBJECT:TabKabelMan:Controls:Add(THIS-OBJECT:SkapKort).
      THIS-OBJECT:TabKabelMan:Controls:Add(THIS-OBJECT:SkapAnm).
      THIS-OBJECT:TabKabelMan:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:TabKabelMan:ImageSize = NEW System.Drawing.Size(40, 40).
      THIS-OBJECT:TabKabelMan:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:TabKabelMan:Name = "TabKabelMan".
      THIS-OBJECT:TabKabelMan:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
      THIS-OBJECT:TabKabelMan:Size = NEW System.Drawing.Size(1149, 603).
      THIS-OBJECT:TabKabelMan:TabIndex = 0.
      THIS-OBJECT:TabKabelMan:TabOrientation = Infragistics.Win.UltraWinTabs.TabOrientation:BottomLeft.
      ultraTab1:TabPage = THIS-OBJECT:IdSkap.
      ultraTab1:Text = "ID".
      ultraTab2:TabPage = THIS-OBJECT:KonsSkap.
      ultraTab2:Text = "SkapKonst".
      ultraTab3:TabPage = THIS-OBJECT:Skapval.
      ultraTab3:Text = "SkapVal".
      ultraTab4:TabPage = THIS-OBJECT:SkapMtrl.
      ultraTab4:Text = "SkapMtrl".
      ultraTab5:TabPage = THIS-OBJECT:SkapKort.
      ultraTab5:Text = "SkapKort".
      ultraTab6:TabPage = THIS-OBJECT:SkapAnm.
      ultraTab6:Text = "SkapAnm".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 6 NO-UNDO.
      arrayvar0[1] = ultraTab1.
      arrayvar0[2] = ultraTab2.
      arrayvar0[3] = ultraTab3.
      arrayvar0[4] = ultraTab4.
      arrayvar0[5] = ultraTab5.
      arrayvar0[6] = ultraTab6.
      THIS-OBJECT:TabKabelMan:Tabs:AddRange(arrayvar0).
      /*  */
      /* ultraTabSharedControlsPage1 */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1".
      THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* KonsSkap */
      /*  */
      THIS-OBJECT:KonsSkap:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:KonsSkap:Name = "KonsSkap".
      THIS-OBJECT:KonsSkap:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* Skapval */
      /*  */
      THIS-OBJECT:Skapval:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:Skapval:Name = "Skapval".
      THIS-OBJECT:Skapval:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* SkapMtrl */
      /*  */
      THIS-OBJECT:SkapMtrl:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:SkapMtrl:Name = "SkapMtrl".
      THIS-OBJECT:SkapMtrl:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* SkapKort */
      /*  */
      THIS-OBJECT:SkapKort:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:SkapKort:Name = "SkapKort".
      THIS-OBJECT:SkapKort:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* SkapAnm */
      /*  */
      THIS-OBJECT:SkapAnm:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:SkapAnm:Name = "SkapAnm".
      THIS-OBJECT:SkapAnm:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* KabelShell */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:TabKabelMan).
      THIS-OBJECT:Name = "KabelShell". 
      THIS-OBJECT:Size = NEW System.Drawing.Size(1149, 603).
      CAST(THIS-OBJECT:TabKabelMan, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabKabelMan:ResumeLayout(FALSE).
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.

	
	DESTRUCTOR PUBLIC KabelShell ( ):

	END DESTRUCTOR.

END CLASS.