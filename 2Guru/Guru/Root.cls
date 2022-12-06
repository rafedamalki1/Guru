 
 /*------------------------------------------------------------------------
    File        : Root
    Purpose     : test
    Syntax      : 
    Description : Objektorienterad Guru-root
    Author(s)   : elpfh
    Created     : Fri Feb 10 08:12:21 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.UserControl.


CLASS Guru.Root: 
	/*------------------------------------------------------------------------------
			Purpose: Hela Guru i 1 klass 																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
	/* Definition av fönsterhanterare */
   {BerKalktt.I}
   DEFINE PUBLIC VARIABLE WindowManager AS Guru.WindowManager NO-UNDO.
   DEFINE PUBLIC VARIABLE DatabaseManager AS Guru.DatabaseManager NO-UNDO.
      /* Definition av modulhanterare */
   DEFINE PUBLIC VARIABLE ModuleManager AS Guru.ModuleManager NO-UNDO.
   DEFINE PUBLIC VARIABLE ModuleModule AS Guru.Module NO-UNDO.
     /* Definition av loggern */ 
   DEFINE PUBLIC VARIABLE Logger AS Guru.Log NO-UNDO.
   
   /* Definition av LanguageManagern */
   DEFINE PUBLIC VARIABLE LanguageManager AS Guru.LanguageManager NO-UNDO.
   
   DEFINE PUBLIC VARIABLE Loaded AS LOGICAL NO-UNDO.	
	CONSTRUCTOR PUBLIC Root (  ):
		SUPER ().
		THIS-OBJECT:setAppStyleSheet("").
		Guru.konstanter:globalroot = THIS-OBJECT.
		THIS-OBJECT:Loaded = FALSE.
		/* Initiera Logger så att vi kan logga huvudmoment i körningen av programmet, och lättare bugga.
		    Skicka alltid med en referens till THIS-OBJECT till alla underklasser, och passa sedan vidare med
		    THIS-OBJECT:Root, på så vis kan man alltid komma till toppen av hierarki-trädet genom den variabeln. */
		THIS-OBJECT:Logger = NEW Guru.Log(THIS-OBJECT).
		
      THIS-OBJECT:Logger:WriteLine("Skapar Guru.DatabaseManager~(" + STRING(THIS-OBJECT) + "~)..").
      THIS-OBJECT:DatabaseManager = NEW Guru.DatabaseManager(THIS-OBJECT).
      
      /* Skapar fönsterhanteraren, denna har hand om fönstret, ribbon, meny. */
      THIS-OBJECT:Logger:WriteLine("Skapar Guru.WindowManager~(" + STRING(THIS-OBJECT) + "~)..").
      THIS-OBJECT:WindowManager = NEW Guru.WindowManager(THIS-OBJECT).
      
				/* Skapar fönsterhanteraren, denna har hand om fönstret, ribbon, meny. */
      THIS-OBJECT:Logger:WriteLine("Skapar Guru.LanguageManager~(" + STRING(THIS-OBJECT) + "~)..").
		THIS-OBJECT:LanguageManager = NEW Guru.LanguageManager(THIS-OBJECT).
		
      /* Skapar modulhanteraren, denna har hand om alla moduler, och vad som visas inuti fönstret (den aktiva modulen) */
      THIS-OBJECT:Logger:WriteLine("Skapar Guru.ModuleManager~(" + STRING(THIS-OBJECT) + "~)..").
		THIS-OBJECT:ModuleManager = NEW Guru.ModuleManager(THIS-OBJECT).	
   	     
	END CONSTRUCTOR.
   CONSTRUCTOR PUBLIC Root ( fake AS LOGICAL ):
      SUPER ().
      THIS-OBJECT:WindowManager = NEW Guru.WindowManager(THIS-OBJECT, FALSE).
      THIS-OBJECT:ModuleManager = NEW Guru.ModuleManager(THIS-OBJECT).  
           
   END CONSTRUCTOR.
   METHOD PUBLIC VOID AvsRoot():
      THIS-OBJECT:WindowManager:wnd:Close().
      DELETE OBJECT THIS-OBJECT:WindowManager NO-ERROR.
      DELETE OBJECT THIS-OBJECT:LanguageManager NO-ERROR.
      DELETE OBJECT THIS-OBJECT:ModuleManager NO-ERROR.
      DELETE OBJECT THIS-OBJECT:DatabaseManager NO-ERROR.
      DELETE OBJECT THIS-OBJECT:Logger NO-ERROR.
      THIS-OBJECT:WindowManager = ?.
      THIS-OBJECT:LanguageManager = ?.
      THIS-OBJECT:ModuleManager = ?.
      THIS-OBJECT:DatabaseManager = ?.
      THIS-OBJECT:Logger = ?.
      
   END METHOD.
   
	DESTRUCTOR PUBLIC Root ( ):
	    /* Destructor, logga och döda saker. */
	    /*
	   THIS-OBJECT:Logger:WriteLine("Avslutar..").
	   */
      DELETE OBJECT THIS-OBJECT:WindowManager NO-ERROR.
      DELETE OBJECT THIS-OBJECT:LanguageManager NO-ERROR.
      DELETE OBJECT THIS-OBJECT:ModuleManager NO-ERROR.
      DELETE OBJECT THIS-OBJECT:DatabaseManager NO-ERROR.
      DELETE OBJECT THIS-OBJECT:Logger NO-ERROR.
      THIS-OBJECT:WindowManager = ?.
      THIS-OBJECT:LanguageManager = ?.
      THIS-OBJECT:ModuleManager = ?.
      THIS-OBJECT:DatabaseManager = ?.
      THIS-OBJECT:Logger = ?.
      
	END DESTRUCTOR.
	
	METHOD PUBLIC VOID BlockInput(sender AS System.Object, e AS Infragistics.Win.UltraWinToolbars.ToolKeyPressEventArgs):
	   e:Handled = TRUE.
   END METHOD.
   
   METHOD PUBLIC VOID startregisterdb(valreg AS CHARACTER).
      DEFINE VARIABLE Modul AS Modules.Register.Register NO-UNDO.
      THIS-OBJECT:Logger:WriteLine("Startar Registermodulen..").
      IF valreg = "SPRAK" THEN DO:
         THIS-OBJECT:DatabaseManager:sprakdb().
         THIS-OBJECT:ModuleManager:InitializeRegister().         
      END.
      THIS-OBJECT:Logger:WriteLine("Initializerar WindowManager..").
      THIS-OBJECT:WindowManager:Initialize().   
      THIS-OBJECT:Logger:WriteLine("Initializerar registermodulen").
      /*
      CAST(THIS-OBJECT:ModuleManager:GetModule("Register"):Control, Modules.Register.Register):Initialize().
      */
      Modul = CAST(THIS-OBJECT:ModuleManager:GetModule("Register"):Control, Modules.Register.Register).
      THIS-OBJECT:GuruRunfile(INPUT STRING(Modul)).
      Modul:Initialize().
      THIS-OBJECT:Loaded = TRUE.
      /* Kör en WAIT-FOR på fönstret, denna bör alltid ligga efter all kod du vill ha med i programmet */
      /*
      THIS-OBJECT:WindowManager:GenerateModuleMenu("").
      */
      THIS-OBJECT:WindowManager:AvslutaKnapp().
      THIS-OBJECT:Logger:WriteLine("Visar fönster..").
      THIS-OBJECT:WindowManager:Wait().  
   END METHOD.
   
   METHOD PUBLIC VOID StartExcel().
      THIS-OBJECT:DatabaseManager:ExcelBlad(). 
   END METHOD. 
   
   METHOD PUBLIC VOID VolymStartKalkylDb().
      THIS-OBJECT:DatabaseManager:KalkylDb("Volym").
   END METHOD.
	METHOD PUBLIC LOGICAL BerStartKalkylDb(INPUT TABLE berkalktt,bernumval AS INTEGER,sidnrval AS INTEGER,visadirekt AS LOGICAL).
      RETURN THIS-OBJECT:DatabaseManager:KalkylDb(INPUT TABLE berkalktt,bernumval,sidnrval,visadirekt).
   END METHOD.
   METHOD PUBLIC VOID StartKalkylDb().
      THIS-OBJECT:DatabaseManager:KalkylDb(). 
   END METHOD.
/*   
   METHOD PUBLIC VOID StartArendeDb().
      THIS-OBJECT:DatabaseManager:ArendeDb(). 
   END METHOD.
  */
   METHOD PUBLIC VOID StartKalkylAdmdb().
      THIS-OBJECT:DatabaseManager:KalkylAdmDb(). 
   END METHOD.

   METHOD PUBLIC VOID StartUtblDb().
      THIS-OBJECT:DatabaseManager:UtblAdmDb(). 
   END METHOD. 
  
   METHOD PUBLIC VOID StartEkgDb().
      THIS-OBJECT:DatabaseManager:EkgDataDb(). 
   END METHOD. 
   METHOD PUBLIC VOID StartKabelAndDb().
   
      THIS-OBJECT:DatabaseManager:KabelAndDb(). 
   END METHOD.
   
   METHOD PUBLIC VOID StartMtrlJmfPrisDb().
      THIS-OBJECT:DatabaseManager:MtrlJmfPrisDb(). 
   END METHOD.
   
   METHOD PUBLIC VOID StartAnvSparrDb().
      THIS-OBJECT:DatabaseManager:AnvSparrDb(). 
   END METHOD.
    
   
   METHOD PUBLIC VOID StartGPLDb(aonrvar AS CHARACTER, delnrvar AS INTEGER, plidvar AS INTEGER).
      THIS-OBJECT:DatabaseManager:GPLDb(aonrvar, delnrvar, plidvar). 
   END METHOD.
   
   METHOD PUBLIC VOID StartStorningDb().
      THIS-OBJECT:DatabaseManager:StorningDb(). 
   END METHOD. 
   METHOD PUBLIC VOID DarwinPlusUserDb():
      THIS-OBJECT:DatabaseManager:DarwinPlusUserDb(). 
   END METHOD.
   METHOD PUBLIC LOGICAL DarwinPlusUser():
       DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE Modul AS Modules.Storning.DarwinPlusUserShell NO-UNDO.
      THIS-OBJECT:DatabaseManager:DarwinPlusUser:Connect().
     
      THIS-OBJECT:ModuleManager:InitializeDarwinPlusUser().
         
      THIS-OBJECT:WindowManager:Initialize().
      THIS-OBJECT:setAppStyleSheet("DarwinPlusUser").
      
      Modul = CAST(THIS-OBJECT:ModuleManager:GetModule("DarwinPlusUser"):Control, Modules.Storning.DarwinPlusUserShell).
      THIS-OBJECT:GuruRunfile(INPUT STRING(Modul)).
      
      Modul:Initialize().
        
      THIS-OBJECT:WindowManager:AvslutaKnapp().
      
      THIS-OBJECT:WindowManager:Wait(). 
   END METHOD. 
  METHOD PUBLIC LOGICAL StartStorning().
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE Modul AS Modules.Storning.StorningShell NO-UNDO.
      THIS-OBJECT:DatabaseManager:Storning:Connect().
      
      THIS-OBJECT:ModuleManager:InitializeStorning().
         
      THIS-OBJECT:WindowManager:Initialize().
      THIS-OBJECT:setAppStyleSheet("Storning").
      
      Modul = CAST(THIS-OBJECT:ModuleManager:GetModule("Storning"):Control, Modules.Storning.StorningShell).
      THIS-OBJECT:GuruRunfile(INPUT STRING(Modul)).
      
      Modul:Initialize().
        
      THIS-OBJECT:WindowManager:AvslutaKnapp().
      THIS-OBJECT:WindowManager:Wait(). 
   END METHOD.
      
   METHOD PUBLIC LOGICAL StartGPL().
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE Modul AS Modules.Projekt.GPL NO-UNDO.
    
      THIS-OBJECT:DatabaseManager:GPL:Connect().
      
      THIS-OBJECT:ModuleManager:InitializeGPL().
         
      THIS-OBJECT:WindowManager:Initialize().
      THIS-OBJECT:setAppStyleSheet("GPL").
      
      Modul = CAST(THIS-OBJECT:ModuleManager:GetModule("GPL"):Control, Modules.Projekt.GPL).
      THIS-OBJECT:GuruRunfile(INPUT STRING(Modul)).
      
      Modul:Initialize().
        
      THIS-OBJECT:WindowManager:AvslutaKnapp().
      
      THIS-OBJECT:WindowManager:Wait(). 
   END METHOD.
   
   METHOD PUBLIC VOID StartKabelAnd(berstarth AS HANDLE).
      /* Initialisera*/
      THIS-OBJECT:DatabaseManager:KabelAnd:Connect(berstarth). 
   END METHOD.
    METHOD PUBLIC VOID StartMtrlJmfPris().
      THIS-OBJECT:DatabaseManager:MtrlJmfPris:ConnectForm(). 
   END METHOD.
   
   
   METHOD PUBLIC VOID VisaSchakt(schackth AS HANDLE):
   END METHOD.
     
   METHOD PUBLIC VOID EkgShell(OUTPUT apph AS HANDLE).
        /* Initialisera dom i omvänd ordning, då båda beror på varandra behöver metoder kallas i särskild ordning, kan nog lösas på bättre sätt */
      
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE Modul AS Modules.EkgData.EkgShell NO-UNDO.
      THIS-OBJECT:Logger:WriteLine("EKG shell ").
      THIS-OBJECT:Logger:WriteLine("Startar Ekgmodulen..").
      THIS-OBJECT:ModuleManager:InitializeEkgShell().
      THIS-OBJECT:Logger:WriteLine("Initializerar WindowManager..").
      THIS-OBJECT:WindowManager:Initialize().
      Modul = CAST(THIS-OBJECT:ModuleManager:GetModule("EkgShell"):Control, Modules.EkgData.EkgShell).
      THIS-OBJECT:GuruRunfile(INPUT STRING(Modul)).
      THIS-OBJECT:Logger:WriteLine("Initializerar EkgShell..").
      
      Modul:Initialize().
        
      
      THIS-OBJECT:Logger:WriteLine("Laddar EkgShell..").
      THIS-OBJECT:WindowManager:AvslutaKnapp().
      
      Modul:StartEkgShell().
      THIS-OBJECT:Logger:WriteLine("Visar fönster..").
      
      THIS-OBJECT:WindowManager:Wait(). 
      apph = DatabaseManager:EkgData:AppServerHandle.
   END METHOD.
   
   
   
   METHOD PUBLIC VOID KalkylAdm(OUTPUT apph AS HANDLE).
        /* Initialisera dom i omvänd ordning, då båda beror på varandra behöver metoder kallas i särskild ordning, kan nog lösas på bättre sätt */
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      /*
      DEFINE VARIABLE Modul AS Modules.Kalkyl.KalkylAdm NO-UNDO.
      */
      DEFINE VARIABLE Modul AS Modules.KalkylAdm.KalkylAdmShell NO-UNDO.
      THIS-OBJECT:Logger:WriteLine("ADm kalkyl ").
      THIS-OBJECT:Logger:WriteLine("Startar kalkylAdmmodulen..").
     
      /*skapar kalkyladmdb mm bla första ribbontab*/
      THIS-OBJECT:ModuleManager:InitializeKalkylAdm().
      THIS-OBJECT:Logger:WriteLine("Initializerar WindowManager..").
      THIS-OBJECT:WindowManager:Initialize().
      IF Guru.konstanter:kalk2sekvar[10] = FALSE AND Guru.konstanter:hoppsekvar[16] = FALSE THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:LanguageManager:GetStringAsMessage(53),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Warning).
         RETURN.
      END.
      /* Debug för kalkyl, Letar efter en modul med namnet Test, och laddar kalkyl i den.*/
      /* Castar om modulen till Kalkyl för att få åtkomst till speciella metoder */
      /*
      Modul = CAST(THIS-OBJECT:ModuleManager:GetModule("KalkylAdm"):Control, Modules.Kalkyl.KalkylAdm).
      */
      Modul = CAST(THIS-OBJECT:ModuleManager:GetModule("KalkylAdm"):Control, Modules.KalkylAdm.KalkylAdmShell).
      THIS-OBJECT:GuruRunfile(INPUT STRING(Modul)).
      THIS-OBJECT:Logger:WriteLine("Initializerar kalkylAdmmodulen..").
      Modul:Initialize().  
      THIS-OBJECT:Logger:WriteLine("Laddar kalkylAdm..").
      /* Kör en WAIT-FOR på fönstret, denna bör alltid ligga efter all kod du vill ha med i programmet */
      /*skapar valmeny till vänster får samma namn som första ribbontab*/
      /*
      THIS-OBJECT:WindowManager:GenerateModuleMenu("KalkylAdm").
      THIS-OBJECT:WindowManager:GenerateModuleMenu("Kalkyl").
      */
      THIS-OBJECT:WindowManager:AvslutaKnapp().
      
      Modul:StartKalkAdm().
      THIS-OBJECT:Logger:WriteLine("Visar fönster..").
      
      THIS-OBJECT:WindowManager:Wait(). 
      apph = DatabaseManager:KalkylAdm:AppServerHandle. 
END METHOD. 
/*
METHOD PUBLIC VOID UtbytListaAdm(OUTPUT apph AS HANDLE):
      /* Initialisera dom i omvänd ordning, då båda beror på varandra behöver metoder kallas i särskild ordning, kan nog lösas på bättre sätt */
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      /*
      DEFINE VARIABLE Modul AS Modules.Kalkyl.KalkylAdm NO-UNDO.
      */
      DEFINE VARIABLE Modul AS Modules.Beredning.UtbytListaAdm NO-UNDO.
      THIS-OBJECT:Logger:WriteLine("ADm Utby ").
      THIS-OBJECT:Logger:WriteLine("Startar UtbytAdmmodulen..").
     
      /*skapar kalkyladmdb mm bla första ribbontab*/
      
      THIS-OBJECT:ModuleManager:InitializeUtbytListaAdm().
      THIS-OBJECT:Logger:WriteLine("Initializerar WindowManager..").
      THIS-OBJECT:WindowManager:Initialize().
      
      /* Debug för kalkyl, Letar efter en modul med namnet Test, och laddar kalkyl i den.*/
      /* Castar om modulen till Kalkyl för att få åtkomst till speciella metoder */
      /*
      Modul = CAST(THIS-OBJECT:ModuleManager:GetModule("KalkylAdm"):Control, Modules.Kalkyl.KalkylAdm).
      */
      Modul = CAST(THIS-OBJECT:ModuleManager:GetModule("UtbytListaAdm"):Control, Modules.Beredning.UtbytListaAdm).
      THIS-OBJECT:GuruRunfile(INPUT STRING(Modul)).
      THIS-OBJECT:Logger:WriteLine("Initializerar UTBYTLISTAdmmodulen..").
      Modul:Initialize().  
      THIS-OBJECT:Logger:WriteLine("Laddar UTBYTLISTAdm..").r
      /* Kör en WAIT-FOR på fönstret, denna bör alltid ligga efter all kod du vill ha med i programmet */
      /*skapar valmeny till vänster får samma namn som första ribbontab*/
      /*
      THIS-OBJECT:WindowManager:GenerateModuleMenu("KalkylAdm").
      THIS-OBJECT:WindowManager:GenerateModuleMenu("Kalkyl").
      */
      THIS-OBJECT:WindowManager:AvslutaKnapp().
      
      Modul:StartUtBytListaAdm().
      THIS-OBJECT:Logger:WriteLine("Visar fönster..").
      
      THIS-OBJECT:WindowManager:Wait(). 
      apph = DatabaseManager:KalkylAdm:AppServerHandle.
END METHOD.
*/
METHOD PUBLIC VOID VolymiSchakt():
   DEFINE VARIABLE Modul AS Modules.Kalkyl.VolymShell NO-UNDO.
   THIS-OBJECT:Logger:WriteLine("Startar kalkylmodulen..").
   THIS-OBJECT:ModuleManager:InitializeVolym().
   THIS-OBJECT:Logger:WriteLine("Initializerar WindowManager..").
   THIS-OBJECT:WindowManager:Initialize().
   THIS-OBJECT:setAppStyleSheet("KalkylIBeredning").
   Modul = CAST(THIS-OBJECT:ModuleManager:GetModule("Volym"):Control, Modules.Kalkyl.VolymShell).
   THIS-OBJECT:GuruRunfile(INPUT STRING(Modul)).
   
   THIS-OBJECT:Logger:WriteLine("Initializerar Volymmodulen..").
   THIS-OBJECT:WindowManager:ShowWindowPropp = TRUE.
   Modul:Initialize().  
   
   THIS-OBJECT:Logger:WriteLine("Laddar volym..").
   
      
   
   THIS-OBJECT:Loaded = TRUE.
   /* Kör en WAIT-FOR på fönstret, denna bör alltid ligga efter all kod du vill ha med i programmet */
   /*
   THIS-OBJECT:WindowManager:GenerateModuleMenu("").
   */
   THIS-OBJECT:WindowManager:AvslutaKnapp().
   THIS-OBJECT:Logger:WriteLine("Visar fönster..").
 
   THIS-OBJECT:WindowManager:Wait().
END METHOD.
     
METHOD PUBLIC VOID KalkyleraiBeredning():
      DEFINE VARIABLE kalknrVar AS INTEGER NO-UNDO.
      DEFINE VARIABLE omradeVar AS CHARACTER NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE Modul AS Modules.Kalkyl.KalkylShell NO-UNDO.
    
      THIS-OBJECT:Logger:WriteLine("Startar kalkylmodulen..").
      THIS-OBJECT:ModuleManager:Initializekalkyl().
      THIS-OBJECT:Logger:WriteLine("Initializerar WindowManager..").
      THIS-OBJECT:WindowManager:Initialize().
      THIS-OBJECT:setAppStyleSheet("KalkylIBeredning").
      Modul = CAST(THIS-OBJECT:ModuleManager:GetModule("Kalkyl"):Control, Modules.Kalkyl.KalkylShell).
      THIS-OBJECT:GuruRunfile(INPUT STRING(Modul)).
      
      THIS-OBJECT:Logger:WriteLine("Initializerar kalkylmodulen..").
      
      Modul:Initialize().  
     
      THIS-OBJECT:Logger:WriteLine("Laddar kalkyl..").
      THIS-OBJECT:DatabaseManager:Kalkyl:BerKalkNr(OUTPUT kalknrVar,OUTPUT omradeVar).
      IF kalknrVar = ? THEN DO:
         THIS-OBJECT:Logger:WriteLine("Kunde inte ladda kalkyl!").
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:LanguageManager:GetStringAsMessage(57),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Warning).
         RETURN.
      END.   
      IF Modul:LoadKalkyl(kalknrVar,omradeVar,FALSE) = FALSE THEN DO:
         THIS-OBJECT:Logger:WriteLine("Kunde inte ladda kalkyl!").
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:LanguageManager:GetStringAsMessage(57),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Warning).
         RETURN.
      END.
     THIS-OBJECT:Loaded = TRUE.
      /* Kör en WAIT-FOR på fönstret, denna bör alltid ligga efter all kod du vill ha med i programmet */
      /*
      THIS-OBJECT:WindowManager:GenerateModuleMenu("").
      */
      THIS-OBJECT:WindowManager:AvslutaKnapp().
      THIS-OBJECT:Logger:WriteLine("Visar fönster..").
    
      THIS-OBJECT:WindowManager:Wait(). 

END METHOD.  
/*ändring av kalkyl skapa kalkyl av berkalk import beredning*/
 METHOD PUBLIC VOID andrakalkyl(INPUT KalkylimportTTh AS HANDLE,INPUT-OUTPUT kalknrVar AS INTEGER,INPUT-OUTPUT omradeVar AS CHARACTER, OUTPUT apph AS HANDLE).
        /* Initialisera dom i omvänd ordning, då båda beror på varandra behöver metoder kallas i särskild ordning, kan nog lösas på bättre sätt */
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE Modul AS Modules.Kalkyl.KalkylShell NO-UNDO.
      
      THIS-OBJECT:Logger:WriteLine("Ändrar kalkyl " + STRING(kalknrVar) + ":" + omradeVar).
      THIS-OBJECT:Logger:WriteLine("Startar kalkylmodulen..").
      THIS-OBJECT:ModuleManager:Initializekalkyl().
      THIS-OBJECT:Logger:WriteLine("Initializerar WindowManager..").
      THIS-OBJECT:WindowManager:Initialize().
      IF THIS-OBJECT:KalkylSek() = FALSE THEN RETURN.
       /* Debug för kalkyl, Letar efter en modul med namnet Test, och laddar kalkyl i den.*/
      /* Castar om modulen till Kalkyl för att få åtkomst till speciella metoder */
     
      IF Guru.GlobalaVariabler:KalkylSomStartiBer = TRUE THEN THIS-OBJECT:setAppStyleSheet("KalkylIBeredning").
      Modul = CAST(THIS-OBJECT:ModuleManager:GetModule("Kalkyl"):Control, Modules.Kalkyl.KalkylShell).
      THIS-OBJECT:GuruRunfile(INPUT STRING(Modul)).
      THIS-OBJECT:Logger:WriteLine("Initializerar kalkylmodulen..").
      Modul:Initialize().
      THIS-OBJECT:Logger:WriteLine("Laddar kalkyl..").
      KalkylimportTTh:FIND-FIRST() NO-ERROR.
      IF KalkylimportTTh:AVAILABLE THEN DO:
         /*ändra kalk med import*/  
         /*KOPIERAR KALKYL FRÅN BEREDNING*/
         IF Modul:LoadKalkyl(KalkylimportTTh,kalknrVar,omradeVar,FALSE) = FALSE THEN DO:
            THIS-OBJECT:Logger:WriteLine("Kunde inte ladda kalkyl!").
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:LanguageManager:GetStringAsMessage(57),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Warning).
            RETURN.
         END.
      END.   
      ELSE DO:
         /*ändra kalk*/
         IF Modul:LoadKalkyl(kalknrVar,omradeVar,FALSE) = FALSE THEN DO:
            THIS-OBJECT:Logger:WriteLine("Kunde inte ladda kalkyl!").
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:LanguageManager:GetStringAsMessage(57),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Warning).
            RETURN.
         END.
      END.    
      KalkylimportTTh:EMPTY-TEMP-TABLE() NO-ERROR.  
      THIS-OBJECT:Loaded = TRUE.
      /* Kör en WAIT-FOR på fönstret, denna bör alltid ligga efter all kod du vill ha med i programmet */
      /*
      THIS-OBJECT:WindowManager:GenerateModuleMenu("").
      */
      THIS-OBJECT:WindowManager:AvslutaKnapp().
      THIS-OBJECT:Logger:WriteLine("Visar fönster..").
      THIS-OBJECT:WindowManager:Wait(). 
      
      THIS-OBJECT:DatabaseManager:Kalkyl:KalkNrGet(OUTPUT kalknrVar,OUTPUT omradeVar).
      apph = DatabaseManager:Kalkyl:AppServerHandle. 
   END METHOD. 
	/*ny kalkyl körs ej!*/
	METHOD PUBLIC VOID andrakalkyl(OUTPUT kalknrVar AS INTEGER,OUTPUT omradeVar AS CHARACTER, OUTPUT apph AS HANDLE).
	
       /* Initialisera dom i omvänd ordning, då båda beror på varandra behöver metoder kallas i särskild ordning, kan nog lösas på bättre sätt */     
      DEFINE VARIABLE Modul AS Modules.Kalkyl.KalkylShell NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      THIS-OBJECT:ModuleManager:Initializekalkyl().
      THIS-OBJECT:WindowManager:Initialize().
      IF THIS-OBJECT:KalkylSek() = FALSE THEN RETURN.
     
      IF THIS-OBJECT:ModuleManager:FindModule("Kalkyl") = TRUE THEN DO:
         /* Castar om modulen till Kalkyl för att få åtkomst till speciella metoder */ 
         /*    
         CAST(THIS-OBJECT:ModuleManager:FoundModule:Control, Modules.Kalkyl.KalkylShell):Initialize().
         */
         
         Modul = CAST(THIS-OBJECT:ModuleManager:FoundModule:Control, Modules.Kalkyl.KalkylShell).
         THIS-OBJECT:GuruRunfile(INPUT STRING(Modul)).
         Modul:Initialize().
    
    
         IF CAST(THIS-OBJECT:ModuleManager:FoundModule:Control, Modules.Kalkyl.KalkylShell):NewKalkyl() = FALSE THEN DO:
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:LanguageManager:GetStringAsMessage(56),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Warning).
            RETURN.
         END.
      END.
      THIS-OBJECT:Loaded = TRUE.
      /* Kör en WAIT-FOR på fönstret, denna bör alltid ligga efter all kod du vill ha med i programmet */
      /*
      THIS-OBJECT:WindowManager:GenerateModuleMenu("").
      */
      THIS-OBJECT:WindowManager:AvslutaKnapp().
      THIS-OBJECT:Logger:WriteLine("Visar fönster..").
      THIS-OBJECT:WindowManager:Wait().
      THIS-OBJECT:DatabaseManager:Kalkyl:KalkNrGet(OUTPUT kalknrVar,OUTPUT omradeVar).
      apph = DatabaseManager:Kalkyl:AppServerHandle. 
   END METHOD.
   
   /*ny kalkyl*/
   METHOD PUBLIC VOID andrakalkyl(OUTPUT kalknrVar AS INTEGER,OUTPUT omradeVar AS CHARACTER, INPUT  kalkdatah AS HANDLE, OUTPUT apph AS HANDLE).
      
       /* Initialisera dom i omvänd ordning, då båda beror på varandra behöver metoder kallas i särskild ordning, kan nog lösas på bättre sätt */     
      DEFINE VARIABLE Modul AS Modules.Kalkyl.KalkylShell NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      THIS-OBJECT:ModuleManager:Initializekalkyl().
      THIS-OBJECT:WindowManager:Initialize().
      IF THIS-OBJECT:KalkylSek() = FALSE THEN RETURN.   
      
      /* Debug för kalkyl, Letar efter en modul med namnet Test, och laddar kalkyl i den.*/
      IF THIS-OBJECT:ModuleManager:FindModule("Kalkyl") = TRUE THEN DO:
         /* Castar om modulen till Kalkyl för att få åtkomst till speciella metoder */     
         /*
         CAST(THIS-OBJECT:ModuleManager:FoundModule:Control, Modules.Kalkyl.KalkylShell):Initialize().
         */
         Modul = CAST(THIS-OBJECT:ModuleManager:FoundModule:Control, Modules.Kalkyl.KalkylShell).
         THIS-OBJECT:GuruRunfile(INPUT STRING(Modul)).
         Modul:Initialize().
         IF CAST(THIS-OBJECT:ModuleManager:FoundModule:Control, Modules.Kalkyl.KalkylShell):NewKalkyl(INPUT kalkdatah) = FALSE THEN DO:
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:LanguageManager:GetStringAsMessage(56),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Warning).
            RETURN.
         END.
      END.
      THIS-OBJECT:Loaded = TRUE.
      /* Kör en WAIT-FOR på fönstret, denna bör alltid ligga efter all kod du vill ha med i programmet */
      /*
      THIS-OBJECT:WindowManager:GenerateModuleMenu("").
      */
      THIS-OBJECT:WindowManager:AvslutaKnapp().
      THIS-OBJECT:Logger:WriteLine("Visar fönster..").
      THIS-OBJECT:WindowManager:Wait().
      THIS-OBJECT:DatabaseManager:Kalkyl:KalkNrGet(OUTPUT kalknrVar,OUTPUT omradeVar).
      
      apph = DatabaseManager:Kalkyl:AppServerHandle. 
   END METHOD.
   METHOD PUBLIC LOGICAL KalkylSek():
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.   
      IF Guru.konstanter:kalk2sekvar[1] = FALSE OR Guru.konstanter:kalk2sekvar[2] = FALSE OR Guru.konstanter:kalk2sekvar[4] = FALSE THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:LanguageManager:GetStringAsMessage(53),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Warning).
         RETURN FALSE.
      END.
      RETURN TRUE.
   END METHOD.
   METHOD PUBLIC VOID VisakalKyl(INPUT extravaldfastth AS HANDLE):
      DEFINE VARIABLE Modul AS Modules.Kalkyl.KalkylShell NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      THIS-OBJECT:ModuleManager:Initializekalkyl().
      THIS-OBJECT:WindowManager:Initialize().
      IF Guru.konstanter:kalk2sekvar[1] = FALSE AND Guru.konstanter:hoppsekvar[16] = FALSE THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:LanguageManager:GetStringAsMessage(53),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Warning).
         RETURN.
      END.   
      
      /* Debug för kalkyl, Letar efter en modul med namnet Test, och laddar kalkyl i den.*/
      IF THIS-OBJECT:ModuleManager:FindModule("Kalkyl") = TRUE THEN DO:
         /* Castar om modulen till Kalkyl för att få åtkomst till speciella metoder */     
         /*
         CAST(THIS-OBJECT:ModuleManager:FoundModule:Control, Modules.Kalkyl.KalkylShell):Initialize().
         */
         Modul = CAST(THIS-OBJECT:ModuleManager:FoundModule:Control, Modules.Kalkyl.KalkylShell).
         THIS-OBJECT:GuruRunfile(INPUT STRING(Modul)).
         Modul:Initialize().
         IF Modul:VisaKalkylLoad(extravaldfastth) = FALSE THEN DO:
            THIS-OBJECT:Logger:WriteLine("Kunde inte ladda kalkyl!").
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:LanguageManager:GetStringAsMessage(57),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Warning).
            RETURN.
         END.
        
      END.
      THIS-OBJECT:Loaded = TRUE.
      /* Kör en WAIT-FOR på fönstret, denna bör alltid ligga efter all kod du vill ha med i programmet */
      /*
      THIS-OBJECT:WindowManager:GenerateModuleMenu("").
      */
      THIS-OBJECT:WindowManager:AvslutaKnapp().
      THIS-OBJECT:Logger:WriteLine("Visar fönster..").
      
      /*
      THIS-OBJECT:WindowManager:Wnd:WindowSizeMax().
      */
      THIS-OBJECT:WindowManager:Wait().
     
   END METHOD.
   METHOD PUBLIC VOID SlaIhopKalkyl(OUTPUT kalknrVar AS INTEGER,OUTPUT omradeVar AS CHARACTER, INPUT  kalkdatah AS HANDLE, OUTPUT apph AS HANDLE):
      DEFINE VARIABLE Modul AS Modules.Kalkyl.KalkylShell NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      
      THIS-OBJECT:ModuleManager:Initializekalkyl().
      THIS-OBJECT:WindowManager:Initialize().
      IF Guru.konstanter:kalk2sekvar[1] = FALSE AND Guru.konstanter:hoppsekvar[16] = FALSE THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:LanguageManager:GetStringAsMessage(53),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Warning).
         RETURN.
      END.   
      IF THIS-OBJECT:ModuleManager:FindModule("Kalkyl") = TRUE THEN DO:
         Modul = CAST(THIS-OBJECT:ModuleManager:FoundModule:Control, Modules.Kalkyl.KalkylShell).
         THIS-OBJECT:GuruRunfile(INPUT STRING(Modul)).
         Modul:Initialize().
         IF Modul:SlaIhopKalkylLoad(kalkdatah) = FALSE THEN DO:
            THIS-OBJECT:Logger:WriteLine("Kunde inte ladda kalkyl!").
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:LanguageManager:GetStringAsMessage(57),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Warning).
            RETURN.
         END.
        
      END.
      THIS-OBJECT:Loaded = TRUE.
      
      THIS-OBJECT:WindowManager:AvslutaKnapp().
      THIS-OBJECT:Logger:WriteLine("Visar fönster..").
      THIS-OBJECT:WindowManager:Wait().
      THIS-OBJECT:DatabaseManager:Kalkyl:KalkNrGet(OUTPUT kalknrVar,OUTPUT omradeVar).
      apph = DatabaseManager:Kalkyl:AppServerHandle. 
   END METHOD.
   METHOD PUBLIC VOID JmfKalkyl(OUTPUT kalknrVar AS INTEGER,OUTPUT omradeVar AS CHARACTER, INPUT  kalkdatah AS HANDLE, OUTPUT apph AS HANDLE):
      DEFINE VARIABLE Modul AS Modules.Kalkyl.KalkylShell NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      
      THIS-OBJECT:ModuleManager:Initializekalkyl().
      THIS-OBJECT:WindowManager:Initialize().
      IF Guru.konstanter:kalk2sekvar[1] = FALSE AND Guru.konstanter:hoppsekvar[16] = FALSE THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:LanguageManager:GetStringAsMessage(53),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Warning).
         RETURN.
      END.   
      IF THIS-OBJECT:ModuleManager:FindModule("Kalkyl") = TRUE THEN DO:
         Modul = CAST(THIS-OBJECT:ModuleManager:FoundModule:Control, Modules.Kalkyl.KalkylShell).
         THIS-OBJECT:GuruRunfile(INPUT STRING(Modul)).
         Modul:Initialize().
         IF Modul:JmfKalkylLoad(kalkdatah) = FALSE THEN DO:
            THIS-OBJECT:Logger:WriteLine("Kunde inte ladda kalkyl!").
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:LanguageManager:GetStringAsMessage(57),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Warning).
            RETURN.
         END.
        
      END.
      THIS-OBJECT:Loaded = TRUE.
      
      THIS-OBJECT:WindowManager:AvslutaKnapp().
      THIS-OBJECT:Logger:WriteLine("Visar fönster..").
      THIS-OBJECT:WindowManager:Wait().
      THIS-OBJECT:DatabaseManager:Kalkyl:KalkNrGet(OUTPUT kalknrVar,OUTPUT omradeVar).
      apph = DatabaseManager:Kalkyl:AppServerHandle. 
   END METHOD.
   /*
   METHOD PUBLIC VOID andraArende(INPUT ArendeimportTTh AS HANDLE,INPUT-OUTPUT kalknrVar AS INTEGER,INPUT-OUTPUT omradeVar AS CHARACTER, OUTPUT apph AS HANDLE).
        /* Initialisera dom i omvänd ordning, då båda beror på varandra behöver metoder kallas i särskild ordning, kan nog lösas på bättre sätt */
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE Modul AS Modules.Arende.ArendeShell NO-UNDO.
      
      THIS-OBJECT:Logger:WriteLine("Ändrar Arende " + STRING(kalknrVar) + ":" + omradeVar).
      THIS-OBJECT:Logger:WriteLine("Startar Arendemodulen..").
      THIS-OBJECT:ModuleManager:InitializeArende().
      THIS-OBJECT:Logger:WriteLine("Initializerar WindowManager..").
      THIS-OBJECT:WindowManager:Initialize().
      
      IF Guru.konstanter:hoppsekvar[16] = FALSE THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:LanguageManager:GetStringAsMessage(53),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Warning).
         RETURN.
      END.
      /* Debug för kalkyl, Letar efter en modul med namnet Test, och laddar kalkyl i den.*/
      /* Castar om modulen till Kalkyl för att få åtkomst till speciella metoder */
      Modul = CAST(THIS-OBJECT:ModuleManager:GetModule("Arende"):Control, Modules.Arende.ArendeShell).
      THIS-OBJECT:GuruRunfile(INPUT STRING(Modul)).
      
      THIS-OBJECT:Logger:WriteLine("Initializerar Arendemodulen..").
      Modul:Initialize().  
      THIS-OBJECT:Logger:WriteLine("Laddar Arende..").
      ArendeimportTTh:FIND-FIRST() NO-ERROR.
      IF ArendeimportTTh:AVAILABLE THEN DO:  
         IF Modul:LoadKalkyl(ArendeimportTTh,kalknrVar,omradeVar,FALSE) = FALSE THEN DO:
            THIS-OBJECT:Logger:WriteLine("Kunde inte ladda Ärende!").
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:LanguageManager:GetStringAsMessage(172),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Warning).
            RETURN.
         END.
      END.   
      ELSE DO:
         IF Modul:LoadKalkyl(kalknrVar,omradeVar,FALSE) = FALSE THEN DO:
            THIS-OBJECT:Logger:WriteLine("Kunde inte ladda Ärende!").
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:LanguageManager:GetStringAsMessage(172),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Warning).
            RETURN.
         END.
      END.    
      ArendeimportTTh:EMPTY-TEMP-TABLE() NO-ERROR.  
      THIS-OBJECT:Loaded = TRUE.
      /* Kör en WAIT-FOR på fönstret, denna bör alltid ligga efter all kod du vill ha med i programmet */
      /*
      THIS-OBJECT:WindowManager:GenerateModuleMenu("").
      */
      THIS-OBJECT:WindowManager:AvslutaKnapp().
      THIS-OBJECT:Logger:WriteLine("Visar fönster..").
      THIS-OBJECT:WindowManager:Wait(). 
      
      THIS-OBJECT:DatabaseManager:Arende:ArendeNrGet(OUTPUT kalknrVar,OUTPUT omradeVar).
      apph = DatabaseManager:Arende:AppServerHandle. 
   END METHOD.

   METHOD PUBLIC VOID andraArende(OUTPUT kalknrVar AS INTEGER,OUTPUT omradeVar AS CHARACTER, OUTPUT apph AS HANDLE).
       /* Initialisera dom i omvänd ordning, då båda beror på varandra behöver metoder kallas i särskild ordning, kan nog lösas på bättre sätt */     
      DEFINE VARIABLE Modul AS Modules.Arende.ArendeShell NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      THIS-OBJECT:ModuleManager:InitializeArende().
      THIS-OBJECT:WindowManager:Initialize().
      IF Guru.konstanter:hoppsekvar[16] = FALSE THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:LanguageManager:GetStringAsMessage(53),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Warning).
         RETURN.
      END.   
      IF THIS-OBJECT:ModuleManager:FindModule("Arende") = TRUE THEN DO:
         /* Castar om modulen till Kalkyl för att få åtkomst till speciella metoder */ 
         
         Modul = CAST(THIS-OBJECT:ModuleManager:FoundModule:Control, Modules.Arende.ArendeShell).
         THIS-OBJECT:GuruRunfile(INPUT STRING(Modul)).
         Modul:Initialize().
         IF CAST(THIS-OBJECT:ModuleManager:FoundModule:Control, Modules.Arende.ArendeShell):NewArende() = FALSE THEN DO:
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:LanguageManager:GetStringAsMessage(171),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Warning).
            RETURN.
         END.
      END.
      THIS-OBJECT:Loaded = TRUE.
      /* Kör en WAIT-FOR på fönstret, denna bör alltid ligga efter all kod du vill ha med i programmet */
      /*
      THIS-OBJECT:WindowManager:GenerateModuleMenu("").
      */
      THIS-OBJECT:WindowManager:AvslutaKnapp().
      THIS-OBJECT:Logger:WriteLine("Visar fönster..").
      THIS-OBJECT:WindowManager:Wait().
      THIS-OBJECT:DatabaseManager:Arende:ArendeNrGet(OUTPUT kalknrVar,OUTPUT omradeVar).
      apph = DatabaseManager:Arende:AppServerHandle. 
   END METHOD.
   
   METHOD PUBLIC VOID andraArende(OUTPUT kalknrVar AS INTEGER,OUTPUT omradeVar AS CHARACTER, INPUT  kalkdatah AS HANDLE, OUTPUT apph AS HANDLE).
       /* Initialisera dom i omvänd ordning, då båda beror på varandra behöver metoder kallas i särskild ordning, kan nog lösas på bättre sätt */     
      DEFINE VARIABLE Modul AS Modules.Arende.ArendeShell NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      THIS-OBJECT:ModuleManager:InitializeArende().
      THIS-OBJECT:WindowManager:Initialize().
      IF Guru.konstanter:hoppsekvar[16] = FALSE THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:LanguageManager:GetStringAsMessage(53),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Warning).
         RETURN.
      END.   
      /* Debug för kalkyl, Letar efter en modul med namnet Test, och laddar kalkyl i den.*/
      IF THIS-OBJECT:ModuleManager:FindModule("Arende") = TRUE THEN DO:
         /* Castar om modulen till Kalkyl för att få åtkomst till speciella metoder */     
        
         Modul = CAST(THIS-OBJECT:ModuleManager:FoundModule:Control, Modules.Arende.ArendeShell).
         THIS-OBJECT:GuruRunfile(INPUT STRING(Modul)).
         Modul:Initialize().
         IF CAST(THIS-OBJECT:ModuleManager:FoundModule:Control, Modules.Arende.ArendeShell):NewArende(INPUT kalkdatah) = FALSE THEN DO:
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:LanguageManager:GetStringAsMessage(171),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Warning).
            RETURN.
         END.
      END.
      
      THIS-OBJECT:Loaded = TRUE.
      /* Kör en WAIT-FOR på fönstret, denna bör alltid ligga efter all kod du vill ha med i programmet */
      /*
      THIS-OBJECT:WindowManager:GenerateModuleMenu("").
      */
      THIS-OBJECT:WindowManager:AvslutaKnapp().
      THIS-OBJECT:Logger:WriteLine("Visar fönster..").
      THIS-OBJECT:WindowManager:Wait().
      THIS-OBJECT:DatabaseManager:Arende:ArendeNrGet(OUTPUT kalknrVar,OUTPUT omradeVar).
      apph = DatabaseManager:Arende:AppServerHandle. 
   END METHOD.
   
   METHOD PUBLIC VOID VisaArende(INPUT extravaldfastth AS HANDLE):
      DEFINE VARIABLE Modul AS Modules.Arende.ArendeShell NO-UNDO.
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      THIS-OBJECT:ModuleManager:InitializeArende().
      THIS-OBJECT:WindowManager:Initialize().
      IF Guru.konstanter:kalk2sekvar[1] = FALSE AND Guru.konstanter:hoppsekvar[16] = FALSE THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:LanguageManager:GetStringAsMessage(53),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Warning).
         RETURN.
      END.   
      
      /* Debug för kalkyl, Letar efter en modul med namnet Test, och laddar kalkyl i den.*/
      IF THIS-OBJECT:ModuleManager:FindModule("Arende") = TRUE THEN DO:
         /* Castar om modulen till Kalkyl för att få åtkomst till speciella metoder */     
         /*
         CAST(THIS-OBJECT:ModuleManager:FoundModule:Control, Modules.Arende.ArendeShell):Initialize().
         */
         Modul = CAST(THIS-OBJECT:ModuleManager:FoundModule:Control, Modules.Arende.ArendeShell).
         THIS-OBJECT:GuruRunfile(INPUT STRING(Modul)).
         Modul:Initialize().
         IF Modul:VisaArendeLoad(extravaldfastth) = FALSE THEN DO:
            THIS-OBJECT:Logger:WriteLine("Kunde inte ladda Ärende!").
            rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:LanguageManager:GetStringAsMessage(172),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Warning).
            RETURN.
         END.
        
      END.
      THIS-OBJECT:Loaded = TRUE.
      /* Kör en WAIT-FOR på fönstret, denna bör alltid ligga efter all kod du vill ha med i programmet */
      /*
      THIS-OBJECT:WindowManager:GenerateModuleMenu("").
      */
      THIS-OBJECT:WindowManager:AvslutaKnapp().
      THIS-OBJECT:Logger:WriteLine("Visar fönster..").
     
      /*
      THIS-OBJECT:WindowManager:Wnd:WindowSizeMax().
      */
      THIS-OBJECT:WindowManager:Wait().
     
   END METHOD.
*/   
   METHOD PUBLIC VOID fakekalkyl().
       /* Initialisera dom i omvänd ordning, då båda beror på varandra behöver metoder kallas i särskild ordning, kan nog lösas på bättre sätt */     
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      
      THIS-OBJECT:ModuleManager:InitializeFake().
     
      THIS-OBJECT:WindowManager:InitializeFake().
      /* Debug för kalkyl, Letar efter en modul med namnet Test, och laddar kalkyl i den.*/
      IF THIS-OBJECT:ModuleManager:FindModule("CombKalkyl") = TRUE THEN DO:
         /* Castar om modulen till Kalkyl för att få åtkomst till speciella metoder */     
      
      END.
      THIS-OBJECT:Loaded = TRUE.
      /* Kör en WAIT-FOR på fönstret, denna bör alltid ligga efter all kod du vill ha med i programmet */
     /*
      THIS-OBJECT:WindowManager:GenerateModuleMenu("").
      */
      THIS-OBJECT:WindowManager:Wait().
       
   END METHOD.
   /*
   METHOD PUBLIC VOID VisaArendStatus(INPUT kalknrVar AS INTEGER,INPUT omradeVar AS CHARACTER, INPUT  kalkdatah AS HANDLE,OUTPUT apph AS HANDLE).
        /* Initialisera dom i omvänd ordning, då båda beror på varandra behöver metoder kallas i särskild ordning, kan nog lösas på bättre sätt */
      DEFINE VARIABLE rrr AS System.Windows.Forms.DialogResult NO-UNDO.
      DEFINE VARIABLE Modul AS Modules.Arende.ArendeShell NO-UNDO.
      THIS-OBJECT:Logger:WriteLine("Ändrar kalkyl " + STRING(kalknrVar) + ":" + omradeVar).
      THIS-OBJECT:Logger:WriteLine("Startar kalkylmodulen..").
      THIS-OBJECT:ModuleManager:InitializeVisaArendStatus().
      THIS-OBJECT:Logger:WriteLine("Initializerar WindowManager..").
      THIS-OBJECT:WindowManager:Initialize().
      
      IF Guru.konstanter:hoppsekvar[16] = FALSE THEN DO:
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:LanguageManager:GetStringAsMessage(53),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Warning).
         RETURN.
      END.
      /* Debug för kalkyl, Letar efter en modul med namnet Test, och laddar kalkyl i den.*/
      /* Castar om modulen till Kalkyl för att få åtkomst till speciella metoder */
      Modul = CAST(THIS-OBJECT:ModuleManager:GetModule("Arende"):Control, Modules.Arende.ArendeShell).
      THIS-OBJECT:GuruRunfile(INPUT STRING(Modul)).
      THIS-OBJECT:Logger:WriteLine("Initializerar Arendemodulen..").
      Modul:InitializeArendeStatus().  
      THIS-OBJECT:Logger:WriteLine("Laddar Arende..").
      IF Modul:VisaArendStatus(kalkdatah,kalknrVar,omradeVar,FALSE) = FALSE THEN DO:
         THIS-OBJECT:Logger:WriteLine("Kunde inte ladda ärende!").
         rrr = System.Windows.Forms.MessageBox:Show(THIS-OBJECT:LanguageManager:GetStringAsMessage(172),"", System.Windows.Forms.MessageBoxButtons:Ok, System.Windows.Forms.MessageBoxIcon:Warning).
         RETURN.
      END.    
       
      THIS-OBJECT:Loaded = TRUE.
      /* Kör en WAIT-FOR på fönstret, denna bör alltid ligga efter all kod du vill ha med i programmet */
      /*
      THIS-OBJECT:WindowManager:GenerateModuleMenu("").
      */
      THIS-OBJECT:WindowManager:AvslutaKnapp().
      THIS-OBJECT:Logger:WriteLine("Visar fönster..").
      THIS-OBJECT:WindowManager:Wait(). 
      apph = DatabaseManager:Arende:AppServerHandle.  
   END METHOD.
    
	*/
	METHOD PUBLIC VOID SoloRegister():
      THIS-OBJECT:ModuleManager:InitializeRegister().
      THIS-OBJECT:WindowManager:Initialize().
      IF THIS-OBJECT:ModuleManager:FindModule("Register") = TRUE THEN DO:
         CAST(THIS-OBJECT:ModuleManager:FoundModule:Control, Modules.Register.Register):Initialize().
      END.
      THIS-OBJECT:Loaded = TRUE.
      /* Kör en WAIT-FOR på fönstret, denna bör alltid ligga efter all kod du vill ha med i programmet */
      /*
      THIS-OBJECT:WindowManager:GenerateModuleMenu("").
      */
      THIS-OBJECT:WindowManager:AvslutaKnapp().
      THIS-OBJECT:Logger:WriteLine("Visar fönster..").
      THIS-OBJECT:WindowManager:Wait(). 
   END METHOD.
   
   METHOD PUBLIC VOID GuruRunfile(modul AS CHARACTER):
      Guru.GlobalaVariabler:globalRunFile = REPLACE(modul,".","\").
   END METHOD.
   METHOD PUBLIC VOID setAppStyleSheet(INPUT icStyleSheet AS CHARACTER):
      DEFINE VARIABLE traffstyle AS LOGICAL NO-UNDO.
     
      IF icStyleSheet = "" THEN DO:
         Infragistics.Win.Office2007ColorTable:CustomBlendColor = System.Drawing.Color:Empty. 
         Infragistics.Win.Office2007ColorTable:ColorScheme = Infragistics.Win.Office2007ColorScheme:DEFAULT. 
         RETURN.
      END.
      IF INDEX(icStyleSheet,".isl") NE 0 THEN DO:
         IF SEARCH(icStyleSheet) NE ? THEN DO:
            traffstyle = TRUE.
            Infragistics.Win.AppStyling.StyleManager:LOAD(SEARCH(icStyleSheet)).
         END.   
      END.
            
      IF traffstyle = FALSE THEN DO: 
         IF icStyleSheet  = "KalkylIBeredning" THEN DO:
            Infragistics.Win.Office2007ColorTable:CustomBlendColor = System.Drawing.Color:BlueViolet.  
            Infragistics.Win.Office2007ColorTable:ColorScheme = Infragistics.Win.Office2007ColorScheme:Blue.
         END.
         IF icStyleSheet  = "GPL" THEN DO:
            Infragistics.Win.Office2007ColorTable:CustomBlendColor = System.Drawing.ColorTranslator:FromHtml("#10c4b2").  
            Infragistics.Win.Office2007ColorTable:ColorScheme = Infragistics.Win.Office2007ColorScheme:Black.
         END.
      END.         
   END METHOD.
END CLASS.