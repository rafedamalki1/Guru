
/*------------------------------------------------------------------------
    File        : DokumentOmGuruProgram.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Oct 25 15:01:51 CEST 2022
    Notes       :
  ----------------------------------------------------------------------*/
  Konstanter.cls 
  innehåller konstanter som laddas på SERVER och klient ska ej ändras 
  innehåller också temptabeller som ej ändras
  innehåller metoder som kan användas över allt
  några mycket bra metopder för att placera knappar  mm
  
  
  GlobalaVariabler.cls  
  innehåller gurus gamla GLOBAL SHARED konstanter och andra temptabeller och konstanter som får ändras men används frekvent
  SharedVariable.cls gör samma saker som  GlobalaVariabler.cls
  
   FakeRoot.cls
    För att göra den nya språkmodulen bakåtkompatibel med gamla guru så körs en instans av denna "fakeroot" som enbart 
    laddar konstanter och klient tabeller hämtas från  TEMPTABELLERAPP.p
  
  TEMPTABELLERAPP.p laddar språk temptabeller samt ALLA meddelanden som finns i cls program!!
  cls windows ex
  KalkClasserStart.p  
     startkalkroot = NEW Guru.Root().
     startkalkroot:startkalkyldb().
     startkalkroot:andrakalkyl(INPUT KalkylimportTTh,INPUT-OUTPUT kalknr,INPUT-OUTPUT omrvar,OUTPUT AppservControll). 
          -Guru.Root.cls 
          METHOD PUBLIC VOID StartKalkylDb().
            THIS-OBJECT:DatabaseManager:KalkylDb(). håller koll på temptt mm 
            
          METHOD PUBLIC VOID andrakalkyl
             THIS-OBJECT:ModuleManager:Initializekalkyl().
                   Modulhanterare                                                     
                   Håller koll på alla moduler, detta "gränssnitt" är rätt om du vill
                  lägga till/välja en annan modul  lägger in ribbon   OBS! Ribbon görs ej grafiskt
             THIS-OBJECT:WindowManager:Initialize().
                  Fönsterhanterare 
                  IF ShowWindowPropp = TRUE THEN THIS-OBJECT:ShowWindow().
                  ELSE WAIT-FOR THIS-OBJECT:Wnd:ShowDialog().
Kör DEBUGGER så ser man             
alternativ START SE JmfBeredningInkop.p
             
cls dialog 
SE run Register\AllaDatabaserStart.p. VISAR ALLA DATABAS ANSLUTNINGAR
för Grids och ribbon se KalkylShell.cls

 METHOD PUBLIC VOID CreateForm():
      
      THIS-OBJECT:CreateGrids().
      THIS-OBJECT:CreateGridFunctions().
      THIS-OBJECT:CreateCombos().
      THIS-OBJECT:CreateRibbon().
      
 övriga controller som FILL-IN knappar drag AND drop mm se 2guru.controls
 Modules.Global. innehåller färdig funktioner kolla där!  
 
 admin program dessa program körs ej INNAN att du vet vad de gör!!!
 ADMDYNBROWSE.W  fisar alla tabeller kan uppdatera tabort lägga till visa poster
 
 Avsluta användare och företag Ny användare nytt företag
 1. Sätt stoppdatum på användaren i sekretess och informera fakturpers om förändringen. Obs ska månadspriset räknas om?
    Datumet ska vara sista den får köra, faktruraavdelningen ska ha sista faktureringsdatum
    När stoppdatum inträffar, ta bort användaren
 
 2  Avsluta företag genomför punkt 1 ovan! Går att göra på första användare i sekretess sen går det att köra via  ADMDYNBROWSE.W 
    Sätt valdbtemp.DBCACHE = "åååå-mm-dd" i företagets VALDBbxxx.I  
    Då syns det när databasen ska stoppas i Välj databas. Det sker inga backupper från stoppdatum
    Ta bort VALDBbxxx.I från VALDBALL.I och WC-CHECK.P  
    kör XCOMPOMNYKUDKOMPILERA.P
    flytta alla program till server
    ta bort db från server rensa backupper utom den sista.
 
 3. Ny användare/ nytt företag 
    Skapa ny databas, lägg IN ny sekretess
    
    Skapa VALDBbxxx.I fil kolla portnr företag gforetag mm
    Lägg IN VALDBbxxx.I i VALDBALL.I och rätt under fil.
    Lägg IN VALDBbxxx.I WC-CHECK.P
    Lägg IN ev logga i LOGGORIN.P. 
    Lägg IN logga i BILDEr-mappen
    kör XCOMPOMNYKUDKOMPILERA.P
    flytta alla program till server
    Starta db på rätt server 
    Lägg IN användare
    Skapa ny VERSION eller installera användare och lägg dit WC-CHECK.P och logga manuelt     
    informera fakturpers om förändringen. Obs ska månadspriset räknas om?
 
 
 
 ALLMANaonrPagaende.P  ändra aonr SPECIAL FÖR AONR
 ALLMANBYTaonr.P  SPECIAL FÖR AONR  
 ALLMaonrbort.P tar bort aonr helt från db MEDDELANDE alla tillhörande tabeller 
 
 AnonymAONR.p gör personer anonyma på det som gäller aonr
 AnonymPkod.p  gör personer anonyma på det som gäller örigt som tidskrivning mm se annan dokumenta tion 
 ALLMANBYT.P BYTER PÅ UNICA NYKLAR I ALLA TABELLER I DATABASEN FÖR ANGIVET VÄRDE TILL ANGIVET VÄRDE FÖR ANGIVET FÄLT I DATABASEN
 
 AONRNAMNKOLL.P kollar indat så det ej har otilåtna tecken
 EPOSTKOLL.P  kollar indat så det ej har otilåtna tecken
 
EXTRATABHMT.P NÄR KOPPLINGAR MELLAN TABELLER BEHÖVS
EXTRADATAHMT.P NÄR EN TABELL SAKNAR FÄLT
EXTRADATAHMTDS.P  EXTRADATA VIA PRODATASET 
EXTRADATSPEC.P SPECIAL FÖR AONR!
STARTADMDB.w för att hantera databaser mm se STARTADMDB.w
STARTADMMEDSKAP.P skapar MEDDELANDE till flera databaser
STARTADMVisaAvSekApp.P visar GURUKOLL EXTRADATA
GURUSUPPORTANV.W FÖR ATT LÄGGA IN GURUSUPPORT USER 
XCOMPALLTSUPER.p KOMPILERAR NY GURU VERSION
XASCIIBOKSTAV.w FÖR KRYPTERING TEXT
XCOMPOMNYKUDKOMPILERA.P DET SOM MÅSTE KOMPILERAS VID NY KUND GÅ IGENOM VARJE PROGRAM SOM KOMPILERAS SÅ DET BLIR RÄTT 
XCOMPWEBcontrols.p FÖR ATT KOMILERA PROGRAM MED LÄGSTA EXCEL VERSION
XLABEL.W FÖR ATT LÄGGA UPP SEKRETESS HOS KUND

STARTload_df.p FÖR ATT LÄGGA TILL TABELLER OCGH FÄLT I DATABASER OBS!
ÄNDRING AVAILABLEDESSA PROGRAM KRÄVER https://knowledgebase.progress.com/articles/Knowledge/P147079
COMPILE XCODE
xcode [ -k key ] -d directory [ files] [ - ]  DVS INGEN KOMPILERING UTAN KRYPTERING 


XCOPYTABLE.W  KOPIERA TABELLER OKÄND STATUS
XCOPYTABLE2.W  KOPIERA TABELLER OKÄND STATUS
XJAMFORTABLE.W jämförtabeller i olika databaser OKÄND STATUS

/*Anders Olsson Elpool i Umeå AB  31 okt 2022 13:48:14*/ 
KOPPLA ELLER KOPIERA KALKYL
    KOPKALK.W körs från AONR OCH PLAN. I PLAN KAN MANAD FÖRA ÖVER PROJEKTET TILL AONR.
    /*vartifran = 2 AONR koppla
vartifran = 3 PLAN koppla
vartifran = 4 AONR kopiera
vartifran = 5 PLAN kopiera
vartifran = 99 PLAN KALK TILL AONR KALK. KOPPLA AONR TILL PLAN MÅSTE VARAAKTIVT
 
*/

kör via MTRLELPOV.W  

RUN DELALLBER.P tar bort alla beredningar inklusive kopplingar
RUN DELALLMARK.P tar bort alla markvärderingar inklusive kopplingar
RUN DELALLDEP.P tar bort alla depåer  inklusive kopplingar
RUN DELALLKALK.P tar bort alla KALKYLER  inklusive kopplingar
    
    KOPPLA FRÅN KOPKALK.W BARA AKTIVA KALKYLER KOPPLINGEN SKER I KALKRUTINK1.P
    KOPIERA FRÅN KOPKALK.W KOPIERINGEN SKER I copycat_UI SÄTTS FN KALKHUV.AKTIV = TRUE.
    KOPIERA FRÅN KALKYLDB.cls   inklusive FRIKalkyl kopieraFRI_UI
                              annars  kopierakalkyl_UI  copycat_UI
                              HAR KALKYLEN AONR FÅR KALKYLEN SAMMA KALKHUV.AKTIV SOM STARTKALKYLEN
                              SAKBNAS AONR BLIR KALKHUV.AKTIV = TRUE   
                              
 autoprogram
 
 AUTO5SPAR.P  körs ej
 AUREG.P AUTOMATREGISTRERING
 AUTOST.P    RUN AUREG.P AUTOMATREGISTRERING                               
 AUTOMISV.P       
      RUN AUTOFLEX.P.
      RUN AUREG.P (INPUT TRUE).
      RUN FORSTTID.P.
      RUN FLAVVAPPNA.P.
      RUN EJGEPOST.P.
      RUN DISPOVMANEPOST.P.
      RUN DISPOVEPOST.P.
      
 
 
 
 AUTOkalmar.p
                
      RUN AUTOST.P.
      RUN BESTTABPRIS.P.
      RUN GKALINDEVIS.P.
      RUN GKAONDEVIS.P.
      RUN GKALMTRL.P.
      RUN FORSTTID.P.        
              
              
AUTOLULE.p
          
      RUN AOUTLUL.P.
      RUN LULEIN.P.
      RUN AUTOST.P. 
      RUN LEVUTLULE.P.
      RUN DEPCHECKLU.P.
             
               
AUTOSU9.p
     RUN SUNDINSIKTAO.p.
     RUN AUTOFLEX.P.
     RUN BESTTABPRIS.P.
     RUN AUTOST.P. 
     RUN FORSTTID.P.
     RUN FAKTKOLL.P.
     RUN FLAVVAPPNA.P.
     RUN EJFEPOST.P.
     RUN EJGEPOST.P.
     RUN AOAVSLEPOST.P.
     RUN EJFEPOST.P.
     RUN GEPOST.P.
     RUN LARM35TMAN.P.
     RUN DISPOVEPOST.P.
     RUN NAPRBER.P.  /* uppdatera pris i beredning om katalog uppdaterad*/
       
AUTOSU9BERGET.p
     RUN AUTOFLEX.P.
     RUN BESTTABPRIS.P.
     RUN AUTOST.P. 
     RUN FORSTTID.P.
     RUN FAKTKOLL.P.
     RUN FLAVVAPPNA.P.
     RUN EJFEPOST.P.
     RUN OBEORDLISTEJGOD.P.
     RUN EJGEPOST.P.
     RUN AOAVSLEPOST.P.
     RUN textut_UI (INPUT "AOAVSLUTKOLL KLAR").                                   
     RUN DISPOVEPOST.P.
     RUN BESTPUNKTSNAT.P.
     RUN NAPRBER.P.  /* uppdatera pris i beredning om katalog uppdaterad*/
  
  AUTOAPPSTARTVATTPRO.P   uppdaterar beredningar körs separat från övriga nattkörningar.
  AUTOVATTPRO.P
     RUN GRANAHLS.P (INPUT 1, INPUT 1).
  AUTOVATTDP.P
       RUN GRANAHLS.P (INPUT 1, INPUT 1).
  
  
  AUTOBODEN.P körs ej ?      
  AUTOELPSALJ.p  elpools sälj databas   
  AUTOELTEL.P  körs ej ?
  AUTOFORS.P  
     RUN NAPRBER.P.  /* uppdatera pris i beredning om katalog uppdaterad*/
 AUTOLAPP.P    körs ej ?
 AUTOONENO.p 
 AUTOREJL.P
 AUTOUMEA.P
    RUN AUTOMARKUM.P.      
 AUTOWWW2EGNAIP.p
        IF valdbtemp.DBNAMN = "SUNDNAT" THEN RUN AUTOSU9BERGET.p (INPUT valdbtemp.FORETAG ).
        IF valdbtemp.DBNAMN = "KRIN" THEN RUN NAPRBER.P.  /* uppdatera pris i beredning om katalog uppdaterad*/
        IF valdbtemp.DBNAMN = "UMBR" THEN RUN AUTOMARKUM.P.  /*skicka meddelande valda datum*/
AUTOWWW2EGNAIPVAL.p   körs ej      


NYBEREU.W                            
 franvart = varifran = 1 NY/ELLER ÄNDRAD DIREKT FRÅN BEREDNINGSMENYN import av beredning
franvart = varifran = 3 NY BERE FRÅN AONRHUVUD
franvart = varifran = 2 ÄNDRAD BERE FRÅN AONRHUVUD 
vart = NYA eller AND DVS NY BEREDNING ELLER ÄNDRA HUVUDET PÅ BEFINTLIG
lnamnex ÄR LEVERANTÖR VALD FRÅM IMPORT
FRÅN 
EXPIMPBERDS.W
   FRÅN BERMENY.P
   IMPORT FRÅN ANNAN BEREDNING
   vart = "NYA".
   lnamnex = LEVERANTÖRENS NAMN I KLARTEXT 
   RUN NYBEREU.W (INPUT 1,INPUT lnamnex ).
   IMPORT FRÅN AVCAD TYPDATOR = 3 = FRÅN AVCAD
   RUN NYBEREU.W (INPUT 1,INPUT "").
BERMENY.P
  RUN NYBEREU.W (INPUT franvart,INPUT "").
     andra_UI
        vart = "AND".
        RUN NYBEREU.W (INPUT franvart,INPUT "").
     btnkopi_UI
        vart = "NYA".  
        RUN NYBEREU.W (INPUT franvart,INPUT "").
     ny_UI      
        vart = "NYA".  
        RUN NYBEREU.W (INPUT franvart,INPUT "").  
  AONRM.P
      NYA
      franvart = 3
      RUN menyberval_UI IN Guru.SharedVariable:btnberh (INPUT bervart,INPUT 3, INPUT aonrvar, INPUT delnrvar, INPUT TABLE berintemp,INPUT-OUTPUT inkopvart). 
          ny_UI      
          vart = "NYA".  
          RUN NYBEREU.W (INPUT franvart,INPUT "").
      ÄNDRA    
          franvart = 2    
          RUN menyberval_UI IN Guru.SharedVariable:btnberh (INPUT bervart,INPUT 2, INPUT aonrvar, INPUT delnrvar, INPUT TABLE berintemp,INPUT-OUTPUT inkopvart).
          andra_UI   
          vart = "AND".
          andra_UI  
          RUN NYBEREU.W (INPUT franvart,INPUT "").
      IMPORT
      vart = "NYA". 
      VIA BERMENY.P  EXPIMPBERDS.W   
   
