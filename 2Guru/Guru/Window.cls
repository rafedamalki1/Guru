 
 /*------------------------------------------------------------------------
    File        : Window
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Fri Feb 10 08:29:20 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.

CLASS Guru.Window INHERITS Form: 
	DEFINE PUBLIC VARIABLE GuruDefaultsTTh AS HANDLE NO-UNDO.
   DEFINE PUBLIC VARIABLE Root AS Guru.Root NO-UNDO.
   /*ska modulen ha blank eller globanv för defauta vären*/ 
      
   DEFINE PRIVATE VARIABLE m_Window_Toolbars_Dock_Area_Top AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
   DEFINE PRIVATE VARIABLE m_Window_Toolbars_Dock_Area_Right AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
   DEFINE PRIVATE VARIABLE m_Window_Toolbars_Dock_Area_Left AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
   DEFINE PRIVATE VARIABLE m_Window_Toolbars_Dock_Area_Bottom AS Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea NO-UNDO.
   DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PUBLIC VARIABLE Window_Fill_Panel AS System.Windows.Forms.Panel NO-UNDO.
   DEFINE PRIVATE VARIABLE ToolbarManager AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO.
	
      	
	CONSTRUCTOR PUBLIC Window ( INPUT r AS Guru.Root ):
       
      SUPER().
            
      InitializeComponent().

      THIS-OBJECT:Root = r.
      
      CAST(THIS-OBJECT:ToolbarManager:Tools["Debug"], Infragistics.Win.UltraWinToolbars.ButtonTool):SharedPropsInternal:Visible = FALSE.
    
     
      IF (PROGRESS = "FULL" AND Guru.Konstanter:globanv = {setuser.I}) /*OR (PROGRESS = "FULL" AND Guru.Konstanter:globanv = "ELPRS")*/ THEN DO:
         CAST(THIS-OBJECT:ToolbarManager:Tools["Debug"], Infragistics.Win.UltraWinToolbars.ButtonTool):SharedPropsInternal:Visible = TRUE.
      END.
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

	END CONSTRUCTOR.
   PROCEDURE getFullVersion EXTERNAL "versioninfo.dll" :
      DEFINE OUTPUT PARAM pcver AS CHARACTER. 
      DEFINE INPUT PARAM pilen AS LONG.
      DEFINE RETURN PARAM iret AS LONG.
   END.


   METHOD PUBLIC Infragistics.Win.UltraWinToolbars.UltraToolbarsManager GetToolbarManager():
      RETURN THIS-OBJECT:ToolbarManager.
   END METHOD.
   METHOD PUBLIC VOID  GetToolbarsizer(t AS Infragistics.Win.UltraWinToolbars.ButtonTool):
       t:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.
      
   END METHOD.
   METHOD VOID SetCursor(INPUT waitvar AS LOGICAL):
      
      IF waitvar = FALSE THEN THIS-OBJECT:Window_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
      ELSE THIS-OBJECT:Window_Fill_Panel:Cursor = System.Windows.Forms.Cursors:WaitCursor.
   END METHOD.
	METHOD PRIVATE VOID InitializeComponent(  ):
		
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      THIS-OBJECT:components = NEW System.ComponentModel.Container().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance16 AS Infragistics.Win.Appearance NO-UNDO.
      appearance16 = NEW Infragistics.Win.Appearance("wut", 553929037).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
      resources = NEW Progress.Util.ResourceManager("Guru.Window").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ribbonTab1 AS Infragistics.Win.UltraWinToolbars.RibbonTab NO-UNDO.
      ribbonTab1 = NEW Infragistics.Win.UltraWinToolbars.RibbonTab("ribbonVerktyg").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ribbonGroup1 AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
      ribbonGroup1 = NEW Infragistics.Win.UltraWinToolbars.RibbonGroup("ribbonGlobal").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE labelTool1 AS Infragistics.Win.UltraWinToolbars.LabelTool NO-UNDO.
      labelTool1 = NEW Infragistics.Win.UltraWinToolbars.LabelTool("meh").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE comboBoxTool1 AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
      comboBoxTool1 = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("ComboBoxTool1").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool1 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool1 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Info").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool2 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool2 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Debug").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE ultraTaskPaneToolbar1 AS Infragistics.Win.UltraWinToolbars.UltraTaskPaneToolbar NO-UNDO.
      ultraTaskPaneToolbar1 = NEW Infragistics.Win.UltraWinToolbars.UltraTaskPaneToolbar("Menu").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool3 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool3 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Control1").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool4 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool4 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Control2").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE labelTool2 AS Infragistics.Win.UltraWinToolbars.LabelTool NO-UNDO.
      labelTool2 = NEW Infragistics.Win.UltraWinToolbars.LabelTool("meh").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE comboBoxTool2 AS Infragistics.Win.UltraWinToolbars.ComboBoxTool NO-UNDO.
      comboBoxTool2 = NEW Infragistics.Win.UltraWinToolbars.ComboBoxTool("ComboBoxTool1").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE valueList1 AS Infragistics.Win.ValueList NO-UNDO.
      valueList1 = NEW Infragistics.Win.ValueList(0).
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool5 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool5 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Info").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance11 AS Infragistics.Win.Appearance NO-UNDO.
      appearance11 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool6 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool6 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("Debug").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance12 AS Infragistics.Win.Appearance NO-UNDO.
      appearance12 = NEW Infragistics.Win.Appearance().
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE buttonTool8 AS Infragistics.Win.UltraWinToolbars.ButtonTool NO-UNDO.
      buttonTool8 = NEW Infragistics.Win.UltraWinToolbars.ButtonTool("ButtonTool1xvc").
      @VisualDesigner.FormMember (NeedsInitialize="true").
      DEFINE VARIABLE appearance13 AS Infragistics.Win.Appearance NO-UNDO.
      appearance13 = NEW Infragistics.Win.Appearance().
      THIS-OBJECT:Window_Fill_Panel = NEW System.Windows.Forms.Panel().
      THIS-OBJECT:ToolbarManager = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsManager(THIS-OBJECT:components).
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Right = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Left = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Bottom = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Top = NEW Infragistics.Win.UltraWinToolbars.UltraToolbarsDockArea().
      CAST(THIS-OBJECT:ToolbarManager, System.ComponentModel.ISupportInitialize):BeginInit().

      THIS-OBJECT:SuspendLayout().
      /*  */
      /* Window_Fill_Panel */
      /*  */
      THIS-OBJECT:Window_Fill_Panel:Cursor = System.Windows.Forms.Cursors:Default.
      THIS-OBJECT:Window_Fill_Panel:Dock = System.Windows.Forms.DockStyle:Fill.
      THIS-OBJECT:Window_Fill_Panel:Location = NEW System.Drawing.Point(8, 160).
      THIS-OBJECT:Window_Fill_Panel:Name = "Window_Fill_Panel".
      THIS-OBJECT:Window_Fill_Panel:Size = NEW System.Drawing.Size(1243, 600).
      THIS-OBJECT:Window_Fill_Panel:TabIndex = 0.
      /*  */
      /* ToolbarManager */
      /*  */
      appearance16:Image = CAST(resources:GetObject("appearance16.Image"), System.Object).
      THIS-OBJECT:ToolbarManager:Appearances:Add(appearance16).
      THIS-OBJECT:ToolbarManager:DesignerFlags = 1.
      THIS-OBJECT:ToolbarManager:DockWithinContainer = THIS-OBJECT.
      THIS-OBJECT:ToolbarManager:DockWithinContainerBaseType = Progress.Util.TypeHelper:GetType("Progress.Windows.Form").
      THIS-OBJECT:ToolbarManager:MenuAnimationStyle = Infragistics.Win.UltraWinToolbars.MenuAnimationStyle:Fade.
      THIS-OBJECT:ToolbarManager:MenuSettings:IsSideStripVisible = Infragistics.Win.DefaultableBoolean:False.
      THIS-OBJECT:ToolbarManager:MenuSettings:ToolDisplayStyle = Infragistics.Win.UltraWinToolbars.ToolDisplayStyle:ImageAndText.
      THIS-OBJECT:ToolbarManager:Ribbon:ApplicationMenuButtonImage = CAST(resources:GetObject("ToolbarManager.Ribbon.ApplicationMenuButtonImage"), System.Drawing.Image).
      ribbonTab1:Caption = "Verktyg".
      ribbonGroup1:Caption = "Globala".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar0 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
      arrayvar0[1] = labelTool1.
      arrayvar0[2] = comboBoxTool1.
      ribbonGroup1:Tools:AddRange(arrayvar0).
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar1 AS Infragistics.Win.UltraWinToolbars.RibbonGroup EXTENT 1 NO-UNDO.
      arrayvar1[1] = ribbonGroup1.
      ribbonTab1:Groups:AddRange(arrayvar1).
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar2 AS Infragistics.Win.UltraWinToolbars.RibbonTab EXTENT 1 NO-UNDO.
      arrayvar2[1] = ribbonTab1.
      THIS-OBJECT:ToolbarManager:Ribbon:NonInheritedRibbonTabs:AddRange(arrayvar2).
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar3 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 2 NO-UNDO.
      arrayvar3[1] = buttonTool1.
      arrayvar3[2] = buttonTool2.
      THIS-OBJECT:ToolbarManager:Ribbon:QuickAccessToolbar:NonInheritedTools:AddRange(arrayvar3).
      THIS-OBJECT:ToolbarManager:Ribbon:Visible = TRUE.
      ultraTaskPaneToolbar1:DockedColumn = 0.
      ultraTaskPaneToolbar1:DockedRow = 0.
      ultraTaskPaneToolbar1:Settings:UseLargeImages = Infragistics.Win.DefaultableBoolean:True.
      ultraTaskPaneToolbar1:Text = "UltraToolbar1".
      ultraTaskPaneToolbar1:Visible = FALSE.
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar4 AS Infragistics.Win.UltraWinToolbars.UltraToolbar EXTENT 1 NO-UNDO.
      arrayvar4[1] = ultraTaskPaneToolbar1.
      THIS-OBJECT:ToolbarManager:Toolbars:AddRange(arrayvar4).
      THIS-OBJECT:ToolbarManager:ToolbarSettings:UseLargeImages = Infragistics.Win.DefaultableBoolean:True.
      buttonTool3:SharedPropsInternal:Caption = "Control1".
      buttonTool4:SharedPropsInternal:Caption = "Control2".
      labelTool2:SharedPropsInternal:Caption = "Guru".
      comboBoxTool2:SharedPropsInternal:Caption = "ComboBoxTool1".
      comboBoxTool2:ValueList = valueList1.
      appearance11:Image = CAST(resources:GetObject("appearance11.Image"), System.Object).
      buttonTool5:SharedPropsInternal:AppearancesSmall:Appearance = appearance11.
      buttonTool5:SharedPropsInternal:Caption = "Info".
      appearance12:Image = CAST(resources:GetObject("appearance12.Image"), System.Object).
      buttonTool6:SharedPropsInternal:AppearancesSmall:Appearance = appearance12.
      buttonTool6:SharedPropsInternal:Caption = "Debug".
      appearance13:Image = CAST(resources:GetObject("appearance13.Image"), System.Object).
      buttonTool8:SharedPropsInternal:AppearancesLarge:Appearance = appearance13.
      buttonTool8:SharedPropsInternal:Caption = "ButtonTool1xvc".
      @VisualDesigner.FormMember (NeedsInitialize="false", InitializeArray="true").
      DEFINE VARIABLE arrayvar5 AS Infragistics.Win.UltraWinToolbars.ToolBase EXTENT 7 NO-UNDO.
      arrayvar5[1] = buttonTool3.
      arrayvar5[2] = buttonTool4.
      arrayvar5[3] = labelTool2.
      arrayvar5[4] = comboBoxTool2.
      arrayvar5[5] = buttonTool5.
      arrayvar5[6] = buttonTool6.
      arrayvar5[7] = buttonTool8.
      THIS-OBJECT:ToolbarManager:Tools:AddRange(arrayvar5).
      THIS-OBJECT:ToolbarManager:UseLargeImagesOnMenu = TRUE.
      THIS-OBJECT:ToolbarManager:ToolClick:Subscribe(THIS-OBJECT:ToolbarManager_ToolClick).
      THIS-OBJECT:ToolbarManager:ToolValueChanged:Subscribe(THIS-OBJECT:ToolbarManager_ToolValueChanged).
      /*  */
      /* m_Window_Toolbars_Dock_Area_Right */
      /*  */
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Right:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Right:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Right:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Right.
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Right:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Right:InitialResizeAreaExtent = 8.
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Right:Location = NEW System.Drawing.Point(1251, 160).
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Right:Name = "m_Window_Toolbars_Dock_Area_Right".
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Right:Size = NEW System.Drawing.Size(8, 600).
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Right:ToolbarsManager = THIS-OBJECT:ToolbarManager.
      /*  */
      /* m_Window_Toolbars_Dock_Area_Left */
      /*  */
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Left:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Left:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Left:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Left.
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Left:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Left:InitialResizeAreaExtent = 8.
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Left:Location = NEW System.Drawing.Point(0, 160).
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Left:Name = "m_Window_Toolbars_Dock_Area_Left".
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Left:Size = NEW System.Drawing.Size(8, 600).
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Left:ToolbarsManager = THIS-OBJECT:ToolbarManager.
      /*  */
      /* m_Window_Toolbars_Dock_Area_Bottom */
      /*  */
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Bottom:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Bottom:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Bottom:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Bottom.
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Bottom:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Bottom:InitialResizeAreaExtent = 8.
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Bottom:Location = NEW System.Drawing.Point(0, 760).
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Bottom:Name = "m_Window_Toolbars_Dock_Area_Bottom".
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Bottom:Size = NEW System.Drawing.Size(1259, 8).
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Bottom:ToolbarsManager = THIS-OBJECT:ToolbarManager.
      /*  */
      /* m_Window_Toolbars_Dock_Area_Top */
      /*  */
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Top:AccessibleRole = System.Windows.Forms.AccessibleRole:Grouping.
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Top:BackColor = System.Drawing.Color:FromArgb(System.Convert:ToInt32(System.Convert:ToByte(191)), System.Convert:ToInt32(System.Convert:ToByte(219)), System.Convert:ToInt32(System.Convert:ToByte(255))).
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Top:DockedPosition = Infragistics.Win.UltraWinToolbars.DockedPosition:Top.
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Top:ForeColor = System.Drawing.SystemColors:ControlText.
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Top:Location = NEW System.Drawing.Point(0, 0).
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Top:Name = "m_Window_Toolbars_Dock_Area_Top".
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Top:Size = NEW System.Drawing.Size(1259, 160).
      THIS-OBJECT:m_Window_Toolbars_Dock_Area_Top:ToolbarsManager = THIS-OBJECT:ToolbarManager.
      /*  */
      /* Window */
      /*  */
      THIS-OBJECT:ClientSize = NEW System.Drawing.Size(1259, 768).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:Window_Fill_Panel).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:m_Window_Toolbars_Dock_Area_Left).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:m_Window_Toolbars_Dock_Area_Right).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:m_Window_Toolbars_Dock_Area_Top).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:m_Window_Toolbars_Dock_Area_Bottom).
      THIS-OBJECT:Name = "Window".
      THIS-OBJECT:Text = "Guru 2.0".
      THIS-OBJECT:WindowSize().
      CAST(THIS-OBJECT:ToolbarManager, System.ComponentModel.ISupportInitialize):EndInit().
      /*
      THIS-OBJECT:ToolbarManager:UseLargeImagesOnToolbar = TRUE.
      */
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.
	
   METHOD PUBLIC VOID WindowSize():
      
      /*fönster storlek*/
      DEFINE VARIABLE xbredd AS INTEGER NO-UNDO.
      DEFINE VARIABLE yhojd AS INTEGER NO-UNDO.
     
      xbredd = SESSION:WORK-AREA-WIDTH-PIXELS. 
      yhojd = SESSION:WORK-AREA-HEIGHT-PIXELS.
      IF xbredd > Guru.Konstanter:globDefaultstorb THEN xbredd = Guru.Konstanter:globDefaultstorb.
      IF yhojd > Guru.Konstanter:globDefaultstorh THEN yhojd = Guru.Konstanter:globDefaultstorh.
      IF Guru.Konstanter:AnvandareTTh = ? THEN.
      ELSE IF Guru.Konstanter:globstorb = 0 THEN DO:
         Guru.Konstanter:AnvandareTTh:FIND-FIRST("WHERE ANVANDARE = '" + Guru.Konstanter:globanv + "'")  NO-ERROR.
         IF Guru.Konstanter:AnvandareTTh:AVAILABLE THEN DO: 
            ASSIGN
            Guru.Konstanter:globstorh = Guru.Konstanter:AnvandareTTh:BUFFER-FIELD("SIDS"):BUFFER-VALUE.
            Guru.Konstanter:globstorb = Guru.Konstanter:AnvandareTTh:BUFFER-FIELD("SIDL"):BUFFER-VALUE.
         END. 
                    
      END.  
      
      IF Guru.Konstanter:alltidmax = TRUE THEN THIS-OBJECT:WindowState = System.Windows.Forms.FormWindowState:Maximized.
      ELSE DO:
         IF Guru.Konstanter:globDefaultstorb <= 500 OR Guru.Konstanter:globDefaultstorh <= 500 THEN  THIS-OBJECT:WindowSizeMax().
         ELSE DO:
            
            THIS-OBJECT:SIZE = NEW System.Drawing.Size(xbredd, yhojd).
            IF Guru.GlobalaVariabler:fonstercol > 0 THEN THIS-OBJECT:LEFT = Guru.GlobalaVariabler:fonstercol.
            IF Guru.GlobalaVariabler:fonsterrow > 0 THEN THIS-OBJECT:TOP = Guru.GlobalaVariabler:fonsterrow.
         END.  
         
      END.
      /*
      THIS-OBJECT:WindowState = System.Windows.Forms.FormWindowState:Maximized.
      */  
   END METHOD.
   METHOD PUBLIC VOID WindowSizeMax():
      THIS-OBJECT:WindowState = System.Windows.Forms.FormWindowState:Maximized.
        
   END METHOD.
   METHOD PUBLIC VOID WindowSizeNormal():
      THIS-OBJECT:WindowState = System.Windows.Forms.FormWindowState:Normal. 
        
   END METHOD.
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ToolbarManager_ToolClick( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinToolbars.ToolClickEventArgs ):
       
      /*anders*/
      THIS-OBJECT:Root:DatabaseManager:Global:SaveActualValues(INPUT THIS-OBJECT:Root:ModuleManager:CurrentModule:name,INPUT Guru.GlobalaVariabler:GuruDefaultAnv,INPUT e:Tool).
      
      IF THIS-OBJECT:Root:ModuleManager:FindModule(e:Tool:Key) = TRUE THEN DO:
         THIS-OBJECT:Root:ModuleManager:SelectModule(e:Tool:Key).
         RETURN.
      END.
      IF e:Tool:Key = "Info" THEN DO:
         THIS-OBJECT:ShowInfo().
      END.
      IF e:Tool:Key = "Debug" THEN DO:
         /*Anders Olsson Elpool i Umeå AB  18 jun 2014 14:26:13 
         Ta inte bort denna debugg! 
         */
         DEBUGGER:INITIATE().
         DEBUGGER:SET-BREAK().
      END.
      
      IF e:Tool:Key = "Avsluta" THEN DO:
         THIS-OBJECT:Close().
      END.   

	END METHOD.


   METHOD PUBLIC VOID SetTitle(titelvar AS CHARACTER):
      THIS-OBJECT:TEXT = titelvar.
   END METHOD.
   METHOD PUBLIC CHARACTER GetTitle():
      RETURN THIS-OBJECT:TEXT .
   END METHOD.
   METHOD PUBLIC VOID ShowInfo():
      DEFINE VARIABLE outanvanv AS CHARACTER NO-UNDO.
      DEFINE VARIABLE outdatornamn AS CHARACTER NO-UNDO.
      
      DEFINE VARIABLE tempfilinfo AS CHARACTER NO-UNDO.
      DEFINE VARIABLE vc          AS CHARACTER FORMAT "x(20)". 
      DEFINE VARIABLE vcnr        AS CHARACTER FORMAT "x(20)". 
      DEFINE VARIABLE vi          AS INTEGER   NO-UNDO.
      DEFINE VARIABLE Urlsite AS CHARACTER NO-UNDO.
      DEFINE VARIABLE Ekgnr AS CHARACTER NO-UNDO.
      vc = FILL( ' ', 256 ).
      IF SEARCH("versioninfo.dll") NE ? THEN RUN getFullVersion ( OUTPUT vc, INPUT 256, OUTPUT vi ). 
      ELSE vc = PROVERSION.
      RUN INLOAPI.P (OUTPUT outanvanv, OUTPUT outdatornamn).
      IF SEARCH(Guru.GlobalaVariabler:globalRunFile + ".cls") NE ? THEN  FILE-INFO:FILE-NAME =  SEARCH(Guru.GlobalaVariabler:globalRunFile + ".cls").
      ELSE FILE-INFO:FILE-NAME =  SEARCH(Guru.GlobalaVariabler:globalRunFile + ".r").
      tempfilinfo = "Filnamn: " + SUBSTRING(FILE-INFO:FILE-NAME,(R-INDEX(FILE-INFO:FILE-NAME,"\") + 1)) + CHR(10).            
      tempfilinfo = tempfilinfo + "Storlek: " + STRING((FILE-INFO:FILE-SIZE) / 1000) + " kB" + CHR(10). 
      tempfilinfo = tempfilinfo + "Senast ändrad: " + STRING(FILE-INFO:FILE-MOD-DATE) + " " + STRING(FILE-INFO:FILE-MOD-TIME,"HH:MM:SS") + CHR(10).
      tempfilinfo = tempfilinfo + "Datornamn: " + TRIM(outdatornamn) + CHR(10).
      tempfilinfo = tempfilinfo + "PC-användare: " + TRIM(outanvanv) + CHR(10).
      IF vc = ? THEN tempfilinfo = tempfilinfo + "Progress: " + PROVERSION + CHR(10). 
      ELSE tempfilinfo = tempfilinfo + "Progress: " + vc + CHR(10).    
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:
         vc = "".
         DEFINE VARIABLE meh AS CHARACTER NO-UNDO.
         /*
         RUN WEBVERSION.P (INPUT 1,INPUT-OUTPUT vc,INPUT-OUTPUT vcnr,OUTPUT meh,OUTPUT Urlsite).
         */
         RUN WEBVERSION.P (INPUT 1,INPUT-OUTPUT vc,INPUT-OUTPUT vcnr,OUTPUT meh).
         RUN WEBPLACE.P (OUTPUT Ekgnr, OUTPUT Urlsite).
         IF vc = "" THEN.
         ELSE IF vc = ? THEN.
         
         ELSE DO:
            IF SESSION:TEMP-DIRECTORY MATCHES "*EKGONWEB*" THEN  tempfilinfo = tempfilinfo + "EKG version: " + Ekgnr + CHR(10).
            ELSE tempfilinfo = tempfilinfo + "Guru version: " + vc + CHR(10).
            tempfilinfo = tempfilinfo + "Url: " + Urlsite + CHR(10).
         END.         
      END.
      tempfilinfo = tempfilinfo + "Temp: " + SESSION:TEMP-DIRECTORY.
      MESSAGE tempfilinfo VIEW-AS ALERT-BOX TITLE "Fil information".  

   END METHOD.

	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID ToolbarManager_ToolValueChanged( INPUT sender AS System.Object, INPUT e AS Infragistics.Win.UltraWinToolbars.ToolEventArgs ):
		/*IF e:Tool:Key = "KalkylRibbonMatris" THEN Cast(THIS-OBJECT:Root:ModuleManager:GetModule("Kalkyl"):Control, Modules.Kalkyl.KalkylShell):UpdateMatris().*/
		/*ANDERS*/
		THIS-OBJECT:Root:DatabaseManager:Global:SaveActualValues(INPUT THIS-OBJECT:Root:ModuleManager:CurrentModule:name,INPUT Guru.GlobalaVariabler:GuruDefaultAnv,INPUT e:Tool).
		RETURN.

	END METHOD.
	
	/* TA BORT SENARAE
   METHOD PRIVATE VOID Window_DragDrop( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.DragEventArgs ):
      DEFINE VARIABLE objFiles AS "System.String[]" NO-UNDO.                                                                                                                                                                                                                                        
      DEFINE VARIABLE j AS INTEGER NO-UNDO.                                                                                   
      MESSAGE "ss"
      VIEW-AS ALERT-BOX.                                                                                                                                                                                                                                                                 
      objFiles = CAST(e:Data:GetData(System.Windows.Forms.DataFormats:FileDrop),"System.String[]").       
                             
         DO J = 1 TO objFiles:Length:
                     MESSAGE
                         objFiles:GetValue(j):ToString()
                         VIEW-AS ALERT-BOX.           
         END.
      RETURN.
   END METHOD.
   
   METHOD PUBLIC VOID DragDropActivate(INPUT omnej AS LOGICAL):
      THIS-OBJECT:AllowDrop = TRUE.
      THIS-OBJECT:DragDrop:Subscribe(THIS-OBJECT:Window_DragDrop). 
      RETURN.
   END METHOD.
   */

	METHOD PUBLIC VOID SelectRibbonTab(INPUT tabKey AS CHARACTER):
      THIS-OBJECT:ToolbarManager:Ribbon:SelectedTab = THIS-OBJECT:ToolbarManager:Ribbon:Tabs[tabKey].
   RETURN.

   END METHOD.
   
	DESTRUCTOR PUBLIC Window ( ):

		IF VALID-OBJECT(components) THEN DO: 
			CAST(components, System.IDisposable):Dispose().
		END.

	END DESTRUCTOR.

END CLASS.


