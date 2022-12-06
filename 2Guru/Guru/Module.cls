
/*------------------------------------------------------------------------
    File        : Module
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Fri Feb 10 11:56:17 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.

CLASS Guru.Module INHERITS System.Object:  
   DEFINE PUBLIC VARIABLE Root AS Guru.Root.
   DEFINE PUBLIC VARIABLE Name AS CHARACTER.
   DEFINE PUBLIC VARIABLE DisplayName AS CHARACTER.
   
   DEFINE PUBLIC VARIABLE Control AS Progress.Windows.UserControl. 
   DEFINE PUBLIC VARIABLE RibbonContext AS Infragistics.Win.UltraWinToolbars.RibbonTab .
   CONSTRUCTOR PUBLIC Module (INPUT r AS Guru.Root, INPUT n AS CHARACTER, INPUT dn AS CHARACTER, INPUT ctrl AS PROGRESS.Windows.UserControl):
     
             /*skapar första ribbontab*/
      THIS-OBJECT:Root = r.
      THIS-OBJECT:Name = n.
      THIS-OBJECT:DisplayName = dn.
      THIS-OBJECT:Control = ctrl.
      THIS-OBJECT:Control:Text = THIS-OBJECT:NAME.
      THIS-OBJECT:RibbonTab(INPUT n, INPUT dn).
         
   END CONSTRUCTOR.
   
   METHOD PUBLIC VOID RibbonTab(INPUT n AS CHARACTER, INPUT dn AS CHARACTER):
     
      THIS-OBJECT:Name = n.
      THIS-OBJECT:DisplayName = dn.

      THIS-OBJECT:RibbonContext = NEW Infragistics.Win.UltraWinToolbars.RibbonTab(THIS-OBJECT:NAME ).
      THIS-OBJECT:RibbonContext:Caption = THIS-OBJECT:DisplayName.
      THIS-OBJECT:RibbonContext:Visible = FALSE.
      
      CAST(THIS-OBJECT:Root:WindowManager:Wnd, Guru.Window):GetToolbarManager():Ribbon:Tabs:Add(THIS-OBJECT:RibbonContext).
      
   END METHOD.
   
END CLASS.