 
 /*------------------------------------------------------------------------
    File        : WindowManager
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Fri Feb 10 08:14:48 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.


CLASS Guru.WindowManager: 
	/*------------------------------------------------------------------------------
			Purpose: F?nsterhanterare 																	  
			Notes:  	Har hand om f?nster, ribbon, meny.																  
	------------------------------------------------------------------------------*/
    /* En pekare till Guru, f?r ?tkomst ?t allt. T.ex genom THIS-OBJECT:Root:ModuleManager:FindModule(). */
   DEFINE PUBLIC VARIABLE Root AS Guru.Root NO-UNDO.
   
   /* Formen/f?nstret i sig*/
   DEFINE PUBLIC VARIABLE Wnd AS Guru.Window NO-UNDO.
   DEFINE PRIVATE VARIABLE ultraToolbarsManager1 AS Infragistics.Win.UltraWinToolbars.UltraToolbarsManager NO-UNDO.
   /* En pekare till den nuvarande valda modulen som en form */
   DEFINE PUBLIC VARIABLE CurrentModuleControl AS Progress.Windows.UserControl. 
   DEFINE PUBLIC VARIABLE MenuInitialized AS LOGICAL INITIAL FALSE NO-UNDO.
   DEFINE PUBLIC PROPERTY ShowWindowPropp AS LOGICAL  NO-UNDO  
      PUBLIC GET.    PUBLIC SET. 
	CONSTRUCTOR PUBLIC WindowManager ( INPUT r AS Guru.Root ):
		SUPER ().
		
		/* Konstruktor, s?tt THIS-OBJECT:Root till "handeln" vi passade som inputvariabel,
		    forts?tt passa vidare f?r Wnd s? att hela programmet f?r access till hela programmet */
		THIS-OBJECT:Root = r.
		
		/* Skapar sj?lva f?nstret */
		THIS-OBJECT:Wnd = NEW Guru.Window(THIS-OBJECT:Root).
		
	END CONSTRUCTOR.
   CONSTRUCTOR PUBLIC WindowManager ( INPUT r AS Guru.Root, INPUT fake AS LOGICAL):
      SUPER ().
      
      /* Konstruktor, s?tt THIS-OBJECT:Root till "handeln" vi passade som inputvariabel,
          forts?tt passa vidare f?r Wnd s? att hela programmet f?r access till hela programmet */
      THIS-OBJECT:Root = r.
      
      /* Skapar sj?lva f?nstret */
      THIS-OBJECT:Wnd = NEW Guru.Window(THIS-OBJECT:Root).
   END CONSTRUCTOR.
	DESTRUCTOR PUBLIC WindowManager ( ):

	END DESTRUCTOR.

   /* Denna kallas n?r anv?ndaren valt en annan modul, s? att f?nstret  uppdaterar vilken modul den visar i Window_Fill_Panel */
   METHOD PUBLIC VOID UpdateModule():
      
      THIS-OBJECT:Wnd:Window_Fill_Panel:Controls:Clear().
      THIS-OBJECT:CurrentModuleControl = THIS-OBJECT:Root:ModuleManager:CurrentModule:Control.
      THIS-OBJECT:Wnd:Window_Fill_Panel:Controls:Add( THIS-OBJECT:CurrentModuleControl ).
      THIS-OBJECT:CurrentModuleControl:Dock = System.Windows.Forms.DockStyle:FILL.
      THIS-OBJECT:CurrentModuleControl:Parent = THIS-OBJECT:Wnd:Window_Fill_Panel.
      THIS-OBJECT:Wnd:GetToolbarManager():Ribbon:SelectedTab:Visible = FALSE.
      THIS-OBJECT:Root:ModuleManager:CurrentModule:RibbonContext:Visible = TRUE.
      THIS-OBJECT:Wnd:GetToolbarManager():Ribbon:SelectedTab =  THIS-OBJECT:Root:ModuleManager:CurrentModule:RibbonContext.

   END METHOD.
   
   METHOD PUBLIC VOID PrepRibbonTool(INPUT t AS Infragistics.Win.UltraWinToolbars.ToolBase):
      THIS-OBJECT:Wnd:GetToolbarManager():Tools:Add(t).     
   END METHOD.
   METHOD PUBLIC VOID PrepRibbonTool(INPUT t AS Infragistics.Win.UltraWinToolbars.ToolBase , INPUT g AS Infragistics.Win.UltraWinToolbars.RibbonGroup):
      THIS-OBJECT:Wnd:GetToolbarManager():Tools:Add(t).
      g:Tools:Add(t).
     /* THIS-OBJECT:Wnd:GetToolbarManager():Tools:Add(g).*/   
   END METHOD.
   
   
   
   /* Denna b?r kallas efter att man lagt till alla moduler under initialiseringen av programmet, den genererar en lista
      i ribbon f?r alla moduler */
   METHOD PUBLIC VOID GenerateModuleMenu(INPUT egetnamn AS CHARACTER):
      DEFINE VARIABLE path AS CHARACTER NO-UNDO.
      DEFINE VARIABLE it AS Infragistics.Win.UltraWinToolbars.ButtonTool.
      IF THIS-OBJECT:MenuInitialized EQ TRUE THEN RETURN.
      /*
      THIS-OBJECT:MenuInitialized = TRUE.
      */
      
      THIS-OBJECT:Root:Logger:WriteLine("Generating menu").
         /*h?r borde alla menyer l?ggas in med hj?lp av hoppsek.*/  
      {WALLMAN\foreach.i System.Object oObject in THIS-OBJECT:Root:ModuleManager:Modules}
         
         /*tar namn fr?n f?rsta ribbontab*/ 
         IF egetnamn = "" THEN DO:
            it = NEW Infragistics.Win.UltraWinToolbars.ButtonTool ( CAST(oObject, Guru.Module):Name).
            it:Key = CAST(oObject, Guru.Module):Name.
            it:CustomizedCaption = CAST(oObject, Guru.Module):DisplayName.
         END. 
         ELSE DO:
            it = NEW Infragistics.Win.UltraWinToolbars.ButtonTool (egetnamn).
            it:Key = egetnamn.
            it:CustomizedCaption = egetnamn.
         END.   
         IF it:KEY = "Kalkyl" THEN path = SEARCH("bilder/kalkico.png").
         ELSE IF it:KEY = "KalkylAdm" THEN path = SEARCH("bilder/kalkico.png").
         ELSE IF it:KEY = "Register" THEN path = SEARCH("bilder/xbtn_kalk.gif").
         

         IF path EQ ? THEN DO:
            MESSAGE "Kunde inte hitta ikon f?r " +  it:KEY + "!"
            VIEW-AS ALERT-BOX.
         END.
         ELSE IF path NE "" THEN DO:
            
            it:SharedPropsInternal:AppearancesLarge:Appearance:Image =  System.Drawing.Image:FromFile(System.IO.Path:GetFullPath(path)).
            
         END.
         THIS-OBJECT:Wnd:GetToolbarManager():Tools:Add(it ).
         THIS-OBJECT:Wnd:GetToolbarManager():Ribbon:ApplicationMenu:ToolAreaLeft:Tools:Add(it ).
         THIS-OBJECT:Root:Logger:WriteLine("Adding " + it:KEY + " and Root is " + STRING(THIS-OBJECT:Root)).
/*         
         /* Skapar och l?gger till st?ngknappen*/
         avslutaKnapp = NEW Infragistics.Win.UltraWinToolbars.ButtonTool ("avsluta").
         avslutaKnapp:Key = "avsluta".
         avslutaKnapp:CustomizedCaption = "Avsluta".
         /*tillf?llig knapp*/
         path = SEARCH("bilder/exitwnd.gif").
         avslutaKnapp:SharedPropsInternal:AppearancesLarge:Appearance:Image =  System.Drawing.Image:FromFile(System.IO.Path:GetFullPath(path)).
   
         THIS-OBJECT:Wnd:GetToolbarManager():Tools:Add(avslutaKnapp).
         THIS-OBJECT:Wnd:GetToolbarManager():Ribbon:ApplicationMenu:ToolAreaLeft:Tools:Add(avslutaKnapp).
  */       
     END.
   END METHOD.
   METHOD PUBLIC VOID AvslutaKnapp():
      DEFINE VARIABLE avslutaKnapp AS Infragistics.Win.UltraWinToolbars.ButtonTool.
      DEFINE VARIABLE path AS CHARACTER NO-UNDO.
      avslutaKnapp = NEW Infragistics.Win.UltraWinToolbars.ButtonTool ("avsluta").
      avslutaKnapp:Key = "avsluta".
      avslutaKnapp:CustomizedCaption = "Avsluta".
      path = SEARCH("bilder/exitwnd.gif").
      avslutaKnapp:SharedPropsInternal:AppearancesLarge:Appearance:Image =  System.Drawing.Image:FromFile(System.IO.Path:GetFullPath(path)).

      THIS-OBJECT:Wnd:GetToolbarManager():Tools:Add(avslutaKnapp).
      THIS-OBJECT:Wnd:GetToolbarManager():Ribbon:ApplicationMenu:ToolAreaLeft:Tools:Add(avslutaKnapp).
   END METHOD.
     
   METHOD PUBLIC VOID Initialize():
      THIS-OBJECT:CurrentModuleControl = THIS-OBJECT:Root:ModuleManager:CurrentModule:Control.
      THIS-OBJECT:Wnd:Window_Fill_Panel:Controls:Add( THIS-OBJECT:CurrentModuleControl ).
      THIS-OBJECT:CurrentModuleControl:Dock = System.Windows.Forms.DockStyle:FILL.
      THIS-OBJECT:CurrentModuleControl:Parent = THIS-OBJECT:Wnd:Window_Fill_Panel.
      THIS-OBJECT:Root:Logger:WriteLine(THIS-OBJECT:CurrentModuleControl:Name + " was loaded").
   END METHOD.
   METHOD PUBLIC VOID InitializeFake():
      THIS-OBJECT:CurrentModuleControl = THIS-OBJECT:Root:ModuleManager:CurrentModule:Control.
      THIS-OBJECT:Wnd:Window_Fill_Panel:Controls:Add( THIS-OBJECT:CurrentModuleControl ).
      THIS-OBJECT:CurrentModuleControl:Dock = System.Windows.Forms.DockStyle:FILL.
      THIS-OBJECT:CurrentModuleControl:Parent = THIS-OBJECT:Wnd:Window_Fill_Panel.
   END METHOD.

   /* Skickar bak?t en public metod f?r att ?ppna f?nstret, ifr?n Window till Root. */
   METHOD PUBLIC VOID Wait():
      THIS-OBJECT:Wnd:SetCursor(FALSE).
      IF Guru.GlobalaVariabler:DoNotOpenWin = TRUE THEN DO:
         Guru.GlobalaVariabler:DoNotOpenWin = FALSE.
         THIS-OBJECT:Wnd:Close(). 
      END.   
     
      ELSE DO:
         /*ShowWindowPropp Man kan f? problem med att Progress f?nster ej visar sig efter cls f?nster. S?ttShowWindowPropp = TRUE.*/
         IF ShowWindowPropp = TRUE THEN THIS-OBJECT:ShowWindow().
         ELSE WAIT-FOR THIS-OBJECT:Wnd:ShowDialog().
     END.
     
  END METHOD.

   METHOD PUBLIC VOID ShowDialog():
      THIS-OBJECT:Wnd:ShowDialog().
   END METHOD.
   
   METHOD PUBLIC VOID ShowWindow():
      WAIT-FOR System.Windows.Forms.Application:Run(THIS-OBJECT:Wnd).
   END METHOD.
   METHOD PUBLIC VOID ExitWindow():
      WAIT-FOR System.Windows.Forms.Application:Exit().
   END METHOD.
   
   METHOD PUBLIC VOID SetCursor(INPUT muswait AS LOGICAL):
      THIS-OBJECT:Wnd:SetCursor(muswait).
      
   END METHOD.
   METHOD PUBLIC VOID Visa():
       Wnd:VISIBLE = TRUE.       
   END METHOD.
   METHOD PUBLIC VOID Visa(INPUT visavar AS LOGICAL):
      /* Wnd:VISIBLE = visavar.*/
       WND:ENABLED = visavar.
   END METHOD.
   


  

END CLASS.