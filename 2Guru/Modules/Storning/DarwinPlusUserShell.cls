 
 /*------------------------------------------------------------------------
    File        : DarwinPlusUserShell
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Mon Nov 13 12:49:18 CET 2017
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.

 
CLASS Modules.Storning.DarwinPlusUserShell INHERITS UserControl: 
	
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraTabSharedControlsPage1 AS Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage NO-UNDO.
   DEFINE PRIVATE VARIABLE TabDWUser AS Infragistics.Win.UltraWinTabControl.UltraTabPageControl NO-UNDO.
   DEFINE PRIVATE VARIABLE TabControlDarwinUsers AS Infragistics.Win.UltraWinTabControl.UltraTabControl NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlDataDb                AS Modules.Storning.DarwinPlusUserDb NO-UNDO.
   DEFINE PUBLIC VARIABLE ControlDarwinPlusUserCon                AS Modules.Storning.DarwinPlusUserCon NO-UNDO.
   DEFINE PUBLIC  VARIABLE Root                         AS Guru.Root                                                      NO-UNDO.  
   DEFINE PRIVATE VARIABLE HmtRubrikerlista             AS Controls.GridRubrikLista                                        NO-UNDO.
		
	CONSTRUCTOR PUBLIC DarwinPlusUserShell (INPUT r AS Guru.Root ):
		
		
      SUPER().
      THIS-OBJECT:Root = r.
      InitializeComponent ( ).
      THIS-OBJECT:ControlDataDb = THIS-OBJECT:Root:DatabaseManager:DarwinPlusUser.
      THIS-OBJECT:ControlDataDb:ControlShell = THIS-OBJECT.
      THIS-OBJECT:HmtRubrikerlista = NEW Controls.GridRubrikLista().
      
      THIS-OBJECT:ControlDataDb:InitializeTTs().
    
     

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
      THIS-OBJECT:TabControlDarwinUsers = NEW Infragistics.Win.UltraWinTabControl.UltraTabControl().
      THIS-OBJECT:ultraTabSharedControlsPage1 = NEW Infragistics.Win.UltraWinTabControl.UltraTabSharedControlsPage().
      THIS-OBJECT:TabDWUser = NEW Infragistics.Win.UltraWinTabControl.UltraTabPageControl().
      CAST(THIS-OBJECT:TabControlDarwinUsers, System.ComponentModel.ISupportInitialize):BeginInit().
      THIS-OBJECT:TabControlDarwinUsers:SuspendLayout().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* TabControlDarwinUsers */
      /*  */
      THIS-OBJECT:TabControlDarwinUsers:Controls:Add(THIS-OBJECT:ultraTabSharedControlsPage1).
      THIS-OBJECT:TabControlDarwinUsers:Controls:Add(THIS-OBJECT:TabDWUser).
      THIS-OBJECT:TabControlDarwinUsers:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:TabControlDarwinUsers:ImageSize = NEW System.Drawing.Size(40, 40).
      THIS-OBJECT:TabControlDarwinUsers:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:TabControlDarwinUsers:Name = "TabControlDarwinUsers".
      THIS-OBJECT:TabControlDarwinUsers:SharedControlsPage = THIS-OBJECT:ultraTabSharedControlsPage1.
      THIS-OBJECT:TabControlDarwinUsers:Size = NEW System.Drawing.Size(1149, 603).
      THIS-OBJECT:TabControlDarwinUsers:TabIndex = 0.
      THIS-OBJECT:TabControlDarwinUsers:TabOrientation = Infragistics.Win.UltraWinTabs.TabOrientation:BottomLeft.
      ultraTab1:TabPage = THIS-OBJECT:TabDWUser.
      ultraTab1:Text = "DWUser".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinTabControl.UltraTab EXTENT 1 NO-UNDO.
      arrayvar0[1] = ultraTab1.
      THIS-OBJECT:TabControlDarwinUsers:Tabs:AddRange(arrayvar0).
      /*  */
      /* ultraTabSharedControlsPage1 */
      /*  */
      THIS-OBJECT:ultraTabSharedControlsPage1:Location = NEW System.Drawing.Point(-10000, -10000).
      THIS-OBJECT:ultraTabSharedControlsPage1:Name = "ultraTabSharedControlsPage1".
      THIS-OBJECT:ultraTabSharedControlsPage1:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* TabDWUser */
      /*  */
      THIS-OBJECT:TabDWUser:AccessibleRole = System.Windows.Forms.AccessibleRole:None.
      THIS-OBJECT:TabDWUser:Location = NEW System.Drawing.Point(1, 1).
      THIS-OBJECT:TabDWUser:Name = "TabDWUser".
      THIS-OBJECT:TabDWUser:Size = NEW System.Drawing.Size(1145, 553).
      /*  */
      /* DarwinPlusUserShell */
      /*  */
      THIS-OBJECT:AutoScaleDimensions = NEW System.Drawing.SizeF(Progress.Util.CastUtil:ToSingle(6), Progress.Util.CastUtil:ToSingle(13)).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:TabControlDarwinUsers).
      THIS-OBJECT:Name = "DarwinPlusUserShell".
      THIS-OBJECT:Size = NEW System.Drawing.Size(1149, 603).
      CAST(THIS-OBJECT:TabControlDarwinUsers, System.ComponentModel.ISupportInitialize):EndInit().
      THIS-OBJECT:TabControlDarwinUsers:ResumeLayout(FALSE).
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.
   METHOD PUBLIC LOGICAL Initialize():
      
      THIS-OBJECT:CreateForm().
      
      RETURN TRUE.
   END METHOD.
   METHOD PUBLIC VOID CreateForm():
      THIS-OBJECT:StartaControl("").
      THIS-OBJECT:CreateGrids().
      THIS-OBJECT:CreateRibbon().
      
     
   END METHOD.
    METHOD PUBLIC VOID CreateGrids():
      IF THIS-OBJECT:ControlDarwinPlusUserCon NE ? THEN DO:
         THIS-OBJECT:ControlDarwinPlusUserCon:CreateGridOrg().
         THIS-OBJECT:ControlDarwinPlusUserCon:CreateGridForetag().
         THIS-OBJECT:ControlDarwinPlusUserCon:CreateGridAnv().
         THIS-OBJECT:ControlDarwinPlusUserCon:CreateGridAnvFore().
      END.
      
   END METHOD.
   METHOD PUBLIC VOID StartaControl(INPUT vad AS CHARACTER):
      THIS-OBJECT:ControlDarwinPlusUserCon = NEW Modules.Storning.DarwinPlusUserCon(THIS-OBJECT:Root).
      THIS-OBJECT:ControlDataDb:InitializeTTs().
      THIS-OBJECT:TabDWUser:Controls:Add(THIS-OBJECT:ControlDarwinPlusUserCon).
      THIS-OBJECT:ControlDarwinPlusUserCon:Dock = System.Windows.Forms.DockStyle:FILL.
   
   END METHOD.
   METHOD PUBLIC VOID CreateRibbon():
   END METHOD.
	DESTRUCTOR PUBLIC DarwinPlusUserShell ( ):

	END DESTRUCTOR.

END CLASS.