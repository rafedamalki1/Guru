 
 /*------------------------------------------------------------------------
    File        : ModuleManager
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Fri Feb 10 10:16:55 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS Guru.ModuleManager: 
	/*------------------------------------------------------------------------------
			Purpose: Modulhanterare 																	  
			Notes:  	Håller koll på alla moduler, detta "gränssnitt" är rätt om du vill
			         lägga till/välja en annan modul	lägger in ribbon												  
	------------------------------------------------------------------------------*/
   DEFINE PUBLIC   PROPERTY rbt                          AS Infragistics.Win.UltraWinToolbars.ButtonTool                   NO-UNDO
	PUBLIC GET. PUBLIC SET.
	DEFINE PUBLIC VARIABLE Root AS Guru.Root NO-UNDO.
	DEFINE PUBLIC VARIABLE Modules AS System.Collections.ArrayList NO-UNDO.
   DEFINE PUBLIC VARIABLE CurrentModule AS Guru.Module NO-UNDO.
   DEFINE PUBLIC VARIABLE FoundModule AS Guru.Module NO-UNDO.
	DEFINE PUBLIC VARIABLE RibbonContextManager AS Infragistics.Win.UltraWinToolbars.RibbonTab .
   DEFINE PUBLIC VARIABLE RibbonTabName AS CHARACTER.
   DEFINE PUBLIC VARIABLE DisplayName AS CHARACTER.
   DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
	CONSTRUCTOR PUBLIC ModuleManager ( INPUT r AS Guru.Root ):
		SUPER ().
		/* Konstruktor, håll alltid regeln med root */
		THIS-OBJECT:Root = r.
		THIS-OBJECT:Modules = NEW System.Collections.ArrayList().
		
	END CONSTRUCTOR.
   
	DESTRUCTOR PUBLIC ModuleManager ( ):
	   
	   {WALLMAN\foreach.i System.Object oObject in THIS-OBJECT:Modules}
	      THIS-OBJECT:CurrentModule = CAST(oObject, Guru.Module).
         DELETE OBJECT  THIS-OBJECT:CurrentModule:control.           
      END.
      IF VALID-OBJECT(components) THEN DO:
         CAST(components, System.IDisposable):Dispose().
      END.
	END DESTRUCTOR.
	
   /* Lägger till en ribbontab*/
   METHOD PUBLIC VOID RibbonTab(INPUT n AS CHARACTER, INPUT dn AS CHARACTER):
      
      THIS-OBJECT:RibbonTabName = n.
      THIS-OBJECT:DisplayName = dn.

      THIS-OBJECT:RibbonContextManager = NEW Infragistics.Win.UltraWinToolbars.RibbonTab(THIS-OBJECT:RibbonTabName ).
      THIS-OBJECT:RibbonContextManager:Caption = THIS-OBJECT:DisplayName.
      THIS-OBJECT:RibbonContextManager:Visible = TRUE .
      CAST(THIS-OBJECT:Root:WindowManager:Wnd, Guru.Window):GetToolbarManager():Ribbon:Tabs:Add(THIS-OBJECT:RibbonContextManager).
      
   END METHOD.
	
	/* Lägger till en modul*/
	METHOD PUBLIC LOGICAL AddModule(INPUT module AS Guru.Module):
	   THIS-OBJECT:Root:Logger:WriteLine("Adding module " + STRING(module) + " with name " + module:Name).
	   THIS-OBJECT:Modules:Add(CAST(module, System.Object)). /*  <- Denna lägger till modulen i arrayen. */
	   THIS-OBJECT:CurrentModule = module.
	END METHOD.
	METHOD PUBLIC LOGICAL AddModuleFake(INPUT module AS Guru.Module):
      THIS-OBJECT:Modules:Add(CAST(module, System.Object)). /*  <- Denna lägger till modulen i arrayen. */
      THIS-OBJECT:CurrentModule = module.
   END METHOD.
	/* Initialisering*/
	METHOD PUBLIC VOID InitializeKalkyl():
      /* Lägg till kalkylmodulen*/
      
      IF Guru.Konstanter:hoppsekvar[3] = TRUE OR  Guru.Konstanter:mtrlsekvar[5] = TRUE THEN DO:
         THIS-OBJECT:Root:Logger:WriteLine("Lägger till modul ~"Kalkyl~"").
         /*lägger till först ribbon + modulen + databasen mm*/
         THIS-OBJECT:AddModule(NEW Guru.Module(THIS-OBJECT:Root, "Kalkyl","Inställningar", NEW Modules.Kalkyl.KalkylShell(THIS-OBJECT:Root))).
          
         /* Välj den första*/
         THIS-OBJECT:Root:Logger:WriteLine("Väljer modul ~"Kalkyl~"").
         THIS-OBJECT:SelectModule("Kalkyl").
         /* Uppdatera fönstret och generera en meny */
         THIS-OBJECT:Root:Logger:WriteLine("Uppdaterar modul").
         THIS-OBJECT:Root:WindowManager:UpdateModule().
      END.
   END METHOD.
   METHOD PUBLIC VOID InitializeVolym():
   /* Lägg till Volymmodulen*/
      THIS-OBJECT:Root:Logger:WriteLine("Lägger till modul ~"Volym~"").
      /*lägger till först ribbon + modulen + databasen mm*/
      THIS-OBJECT:AddModule(NEW Guru.Module(THIS-OBJECT:Root, "Volym","Grunddata", NEW Modules.Kalkyl.VolymShell(THIS-OBJECT:Root))).
       
      /* Välj den första*/
      THIS-OBJECT:Root:Logger:WriteLine("Väljer modul ~"Volym~"").
      THIS-OBJECT:SelectModule("Volym").
      /* Uppdatera fönstret och generera en meny */
      THIS-OBJECT:Root:Logger:WriteLine("Uppdaterar modul").
      THIS-OBJECT:Root:WindowManager:UpdateModule().
   END METHOD.
   
   METHOD PUBLIC VOID InitializeFake():
      /* Lägg till kalkylmodulen*/
      /*
      THIS-OBJECT:AddModuleFake(NEW Guru.Module(THIS-OBJECT:Root, "CombKalkyl", "", NEW Tester.Kalkyl.CombKalkyl(THIS-OBJECT:Root))).
      THIS-OBJECT:Root:WindowManager:UpdateModule().
      */
   END METHOD.
   /* Initialisering*/
   /*
   METHOD PUBLIC VOID InitializeArende():
      /* Lägg till arendemodulen*/
      IF Guru.Konstanter:hoppsekvar[16] = TRUE THEN DO:
         THIS-OBJECT:Root:Logger:WriteLine("Lägger till modul ~"Ärende~"").
         /*lägger till först ribbon + modulen + databasen mm*/
         THIS-OBJECT:AddModule(NEW Guru.Module(THIS-OBJECT:Root, "Arende", Guru.GlobalaVariabler:KalkArendeText, NEW Modules.Arende.ArendeShell(THIS-OBJECT:Root))).
          
         /* Välj den första*/ 
         THIS-OBJECT:Root:Logger:WriteLine("Väljer modul ~"Ärende~"").
         THIS-OBJECT:SelectModule("Arende").
         /* Uppdatera fönstret och generera en meny */
         THIS-OBJECT:Root:Logger:WriteLine("Uppdaterar modul").
         THIS-OBJECT:Root:WindowManager:UpdateModule().
      END.
   END METHOD.
   METHOD PUBLIC VOID InitializeVisaArendStatus():
      /* Lägg till kalkylmodulen*/
      IF Guru.Konstanter:hoppsekvar[16] = TRUE THEN DO:
         THIS-OBJECT:Root:Logger:WriteLine("Lägger till modul ~"Ärende~"").
         THIS-OBJECT:AddModule(NEW Guru.Module(THIS-OBJECT:Root, "Arende", Guru.GlobalaVariabler:KalkArendeText, NEW Modules.Arende.ArendeShell(THIS-OBJECT:Root))).
         /* Välj den första*/
         THIS-OBJECT:Root:Logger:WriteLine("Väljer modul ~"Ärende~"").
         THIS-OBJECT:SelectModule("Arende").
         /* Uppdatera fönstret och generera en meny */
         THIS-OBJECT:Root:Logger:WriteLine("Uppdaterar modul").
         THIS-OBJECT:Root:WindowManager:UpdateModule().
      END.
   END METHOD.
   */
   
   METHOD PUBLIC VOID InitializeKalkylAdm():
      /* Lägg till kalkylmodulen*/
      IF Guru.Konstanter:hoppsekvar[3] = TRUE THEN DO:
         THIS-OBJECT:Root:Logger:WriteLine("Lägger till modul ~"KalkylAdm~"").
         /*lägger till först ribbon + modulen + databasen mm*/
         /*
         THIS-OBJECT:AddModule(NEW Guru.Module(THIS-OBJECT:Root, "KalkylAdm", "Kalkylkataloger", NEW Modules.Kalkyl.KalkylAdm(THIS-OBJECT:Root))).
        */
        THIS-OBJECT:AddModule(NEW Guru.Module(THIS-OBJECT:Root, "KalkylAdm", "Kalkylkataloger", NEW Modules.KalkylAdm.KalkylAdmShell(THIS-OBJECT:Root))).
        /*flera ribbontabbar här eller inne i modulen*/
        /*
         THIS-OBJECT:RibbonTab("kkkc","pcpp").
         */
                /* Välj den första*/
         THIS-OBJECT:Root:Logger:WriteLine("Väljer modul ~"KalkylAdm~"").
         THIS-OBJECT:SelectModule("KalkylAdm").
         /* Uppdatera fönstret och generera en meny */
         THIS-OBJECT:Root:Logger:WriteLine("Uppdaterar modul").
         THIS-OBJECT:Root:WindowManager:UpdateModule().
         
      END.
      
   END METHOD.
   METHOD PUBLIC VOID InitializeUtbytListaAdm():
       THIS-OBJECT:Root:Logger:WriteLine("Lägger till modul ~"UTBYTAdm~"").
     THIS-OBJECT:AddModule(NEW Guru.Module(THIS-OBJECT:Root, "Listor", "Listor", NEW Modules.KalkylAdm.KalkylAdmShell(THIS-OBJECT:Root))).
             /* Välj den första*/
      THIS-OBJECT:Root:Logger:WriteLine("Väljer modul ~"Listor~"").
      THIS-OBJECT:SelectModule("UtbytListaAdm").
      /* Uppdatera fönstret och generera en meny */
      THIS-OBJECT:Root:Logger:WriteLine("Uppdaterar modul").
      THIS-OBJECT:Root:WindowManager:UpdateModule().
      
   END METHOD.
   METHOD PUBLIC VOID InitializeEkgShell():
      /* Lägg till Ekgmodulen*/
      THIS-OBJECT:Root:Logger:WriteLine("Lägger till modul ~"Ekg~"").
      /*lägger till först ribbon + modulen + databasen mm*/
     
      THIS-OBJECT:AddModule(NEW Guru.Module(THIS-OBJECT:Root, "EkgShell", "EBR Kostnadskataloger", NEW Modules.EkgData.EkgShell(THIS-OBJECT:Root))).
        
        /*flera ribbontabbar här eller inne i modulen*/
                /* Välj den första*/
      THIS-OBJECT:Root:Logger:WriteLine("Väljer modul ~"Ekg~"").
      THIS-OBJECT:SelectModule("EkgShell").
      /* Uppdatera fönstret och generera en meny */
      THIS-OBJECT:Root:Logger:WriteLine("Uppdaterar modul").
      THIS-OBJECT:Root:WindowManager:UpdateModule().
      
   END METHOD.
   METHOD PUBLIC VOID InitializeRegister():
      IF Guru.Konstanter:hoppsekvar[9] = TRUE THEN  DO:
         /* Lägg till kalkylmodulen*/
         THIS-OBJECT:AddModule(NEW Guru.Module(THIS-OBJECT:Root, "Register", "Register", NEW Modules.Register.Register(THIS-OBJECT:Root))).
         /* Välj den första*/
         THIS-OBJECT:SelectModule("Register").
         /* Uppdatera fönstret och generera en meny */
         THIS-OBJECT:Root:WindowManager:UpdateModule().
      END.
   END METHOD.
   
   METHOD PUBLIC VOID InitializeMark():
      IF Guru.Konstanter:hoppsekvar[12] = TRUE THEN DO:
       /*
         /* Lägg till kalkylmodulen*/
         
         THIS-OBJECT:AddModule(NEW Guru.Module(THIS-OBJECT:Root, "Mark", "Markvärdering", NEW Modules.Mark.Markvard(THIS-OBJECT:Root))).
         /* Välj den första*/
         THIS-OBJECT:SelectModule("Mark").
         /* Uppdatera fönstret och generera en meny */
         THIS-OBJECT:Root:WindowManager:UpdateModule().
     */
      END.

   END METHOD.
   
   METHOD PUBLIC VOID InitializeGPL():
      /* Lägg till modulen*/
      THIS-OBJECT:Root:Logger:WriteLine("Lägger till modul ~"GPL~"").
      /*lägger till först ribbon + modulen + databasen mm*/
     
      THIS-OBJECT:AddModule(NEW Guru.Module(THIS-OBJECT:Root, "GPL", "GPL", NEW Modules.Projekt.GPL(THIS-OBJECT:Root))).
        
        /*flera ribbontabbar här eller inne i modulen*/
                /* Välj den första*/
      THIS-OBJECT:Root:Logger:WriteLine("Väljer modul ~"GPL~"").
      THIS-OBJECT:SelectModule("GPL").
      /* Uppdatera fönstret och generera en meny */
      THIS-OBJECT:Root:Logger:WriteLine("Uppdaterar modul").
      THIS-OBJECT:Root:WindowManager:UpdateModule().
      
   END METHOD.
   
   METHOD PUBLIC VOID InitializeStorning():
      /* Lägg till Ekgmodulen*/
      THIS-OBJECT:Root:Logger:WriteLine("Lägger till modul ~"Storning~"").
      /*lägger till först ribbon + modulen + databasen mm*/
     
      THIS-OBJECT:AddModule(NEW Guru.Module(THIS-OBJECT:Root, "Storning", "Storning", NEW Modules.Storning.StorningShell(THIS-OBJECT:Root))).
        
        /*flera ribbontabbar här eller inne i modulen*/
                /* Välj den första*/
      THIS-OBJECT:Root:Logger:WriteLine("Väljer modul ~"Storning~"").
      THIS-OBJECT:SelectModule("Storning").
      /* Uppdatera fönstret och generera en meny */
      THIS-OBJECT:Root:Logger:WriteLine("Uppdaterar modul").
      THIS-OBJECT:Root:WindowManager:UpdateModule().
      
   END METHOD.
   METHOD PUBLIC VOID InitializeDarwinPlusUser():
      /* Lägg till Ekgmodulen*/
      THIS-OBJECT:Root:Logger:WriteLine("Lägger till modul ~"DarwinPlusUser~"").
      /*lägger till först ribbon + modulen + databasen mm*/
     
      THIS-OBJECT:AddModule(NEW Guru.Module(THIS-OBJECT:Root, "DarwinPlusUser", "DarwinPlusUser", NEW Modules.Storning.DarwinPlusUserShell(THIS-OBJECT:Root))).
        
        /*flera ribbontabbar här eller inne i modulen*/
                /* Välj den första*/
      THIS-OBJECT:Root:Logger:WriteLine("Väljer modul ~"DarwinPlusUser~"").
      THIS-OBJECT:SelectModule("DarwinPlusUser").
      /* Uppdatera fönstret och generera en meny */
      THIS-OBJECT:Root:Logger:WriteLine("Uppdaterar modul").
      THIS-OBJECT:Root:WindowManager:UpdateModule().
      
   END METHOD.
	
   /* Väljer modul. */
	METHOD PUBLIC LOGICAL SelectModule(INPUT modulename AS CHARACTER):
      /* Avbryt om det inte finns några moduler än */	   
	   IF THIS-OBJECT:Modules:Count < 1 THEN DO:
	      RETURN FALSE.
      END.
      ELSE DO:
         /* Avbryt om modulen redan är vald */
         IF THIS-OBJECT:CurrentModule:Name = modulename THEN DO:
            RETURN FALSE.
         END. 
      END.

      /* Gå igenom alla moduler*/
      {WALLMAN\foreach.i System.Object oObject in THIS-OBJECT:Modules}
         /* Om modulens namn är lika med namnet vi söker, sätt till current, uppdatera windowmanager och returnera true.*/
         IF CAST(oObject, Guru.Module):NAME = modulename THEN DO:
            THIS-OBJECT:CurrentModule = CAST(oObject, Guru.Module).
            THIS-OBJECT:Root:WindowManager:UpdateModule().
            RETURN TRUE.
         END.
      END.
      
      /* Hittades inte modulename? Returnera false */
      RETURN FALSE.

	END METHOD.
	
	/* Kolla om en modul finns i arrayen, bör användas enligt:
	        IF ModuleManager:FindModule("ModulNamn") = TRUE THEN DO:
	           ... Gör något med > ModuleManager:FoundModule < ...
           END.
	*/
	METHOD PUBLIC LOGICAL FindModule(INPUT modulename AS CHARACTER):
      /* Gå igenom alla moduler*/
      {WALLMAN\foreach.i System.Object oObject in THIS-OBJECT:Modules}
         /* Om modulens namn är lika med namnet vi söker, sätt till found,  returnera true.*/
         IF CAST(oObject, Guru.Module):NAME = modulename THEN DO:
            THIS-OBJECT:FoundModule = CAST(oObject, Guru.Module).
            RETURN TRUE.
         END.
      END.
      /* Hittades inte? returnera false */      
      RETURN FALSE.
   END METHOD.
	
	/* Returnerar modul vid namn modulename */
   METHOD PUBLIC Guru.Module GetModule(INPUT modulename AS CHARACTER):
      /* Gå igenom alla moduler*/
      {WALLMAN\foreach.i System.Object oObject in THIS-OBJECT:Modules}
         /* Om modulens namn är lika med namnet vi söker, returnera modul.*/
         IF CAST(oObject, Guru.Module):NAME = modulename THEN DO:
            RETURN CAST(oObject, Guru.Module).
         END.
      END.
      /* Hittades inte? returnera ?*/     
      System.Windows.Forms.MessageBox:Show("Modul hittades inte."). 
      RETURN ?.
   END METHOD.
   
	METHOD PUBLIC VOID RibbonImage(INPUT vad AS INTEGER, INPUT rbt1 AS Infragistics.Win.UltraWinToolbars.ButtonTool,INPUT path AS CHARACTER):
      rbt = rbt1.
      path = SEARCH(path).
      IF path NE ? THEN DO:
         IF vad = 1 THEN THIS-OBJECT:rbt:SharedPropsInternal:AppearancesSmall:Appearance:Image = System.Drawing.Image:FromFile(System.IO.Path:GetFullPath(path)).
         IF vad = 2 THEN THIS-OBJECT:rbt:SharedPropsInternal:AppearancesLarge:Appearance:Image = System.Drawing.Image:FromFile(System.IO.Path:GetFullPath(path)).                  
      END.
      IF vad = 4 THEN THIS-OBJECT:rbt:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Large.  
      IF vad = 5 THEN THIS-OBJECT:rbt:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:Normal.
      IF vad = 6 THEN THIS-OBJECT:rbt:InstanceProps:PreferredSizeOnRibbon = Infragistics.Win.UltraWinToolbars.RibbonToolSize:ImageOnly.
      IF vad = 7 THEN.
        
   END METHOD.
	
	METHOD PUBLIC VOID RibbonImageBackground(INPUT rbt1 AS Infragistics.Win.UltraWinToolbars.ButtonTool,INPUT path AS CHARACTER):
      rbt = rbt1.
      path = SEARCH(path).
      IF path = ? THEN RETURN.
      THIS-OBJECT:rbt:SharedPropsInternal:AppearancesLarge:Appearance:ImageBackground = System.Drawing.Image:FromFile(System.IO.Path:GetFullPath(path)).                  
        
   END METHOD.
	
	
END CLASS.