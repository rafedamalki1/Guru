
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
  inneh?ller konstanter som laddas p? SERVER och klient ska ej ?ndras 
  inneh?ller ocks? temptabeller som ej ?ndras
  inneh?ller metoder som kan anv?ndas ?ver allt
  n?gra mycket bra metopder f?r att placera knappar  mm
  
  
  GlobalaVariabler.cls  
  inneh?ller gurus gamla GLOBAL SHARED konstanter och andra temptabeller och konstanter som f?r ?ndras men anv?nds frekvent
  SharedVariable.cls g?r samma saker som  GlobalaVariabler.cls
  
   FakeRoot.cls
    F?r att g?ra den nya spr?kmodulen bak?tkompatibel med gamla guru s? k?rs en instans av denna "fakeroot" som enbart 
    laddar konstanter och klient tabeller h?mtas fr?n  TEMPTABELLERAPP.p
  
  TEMPTABELLERAPP.p laddar spr?k temptabeller samt ALLA meddelanden som finns i cls program!!
  cls windows ex
  KalkClasserStart.p  
     startkalkroot = NEW Guru.Root().
     startkalkroot:startkalkyldb().
     startkalkroot:andrakalkyl(INPUT KalkylimportTTh,INPUT-OUTPUT kalknr,INPUT-OUTPUT omrvar,OUTPUT AppservControll). 
          -Guru.Root.cls 
          METHOD PUBLIC VOID StartKalkylDb().
            THIS-OBJECT:DatabaseManager:KalkylDb(). h?ller koll p? temptt mm 
            
          METHOD PUBLIC VOID andrakalkyl
             THIS-OBJECT:ModuleManager:Initializekalkyl().
                   Modulhanterare                                                     
                   H?ller koll p? alla moduler, detta "gr?nssnitt" ?r r?tt om du vill
                  l?gga till/v?lja en annan modul  l?gger in ribbon   OBS! Ribbon g?rs ej grafiskt
             THIS-OBJECT:WindowManager:Initialize().
                  F?nsterhanterare 
                  IF ShowWindowPropp = TRUE THEN THIS-OBJECT:ShowWindow().
                  ELSE WAIT-FOR THIS-OBJECT:Wnd:ShowDialog().
K?r DEBUGGER s? ser man             
alternativ START SE JmfBeredningInkop.p
             
cls dialog 
SE run Register\AllaDatabaserStart.p. VISAR ALLA DATABAS ANSLUTNINGAR
f?r Grids och ribbon se KalkylShell.cls

 METHOD PUBLIC VOID CreateForm():
      
      THIS-OBJECT:CreateGrids().
      THIS-OBJECT:CreateGridFunctions().
      THIS-OBJECT:CreateCombos().
      THIS-OBJECT:CreateRibbon().
      
 ?vriga controller som FILL-IN knappar drag AND drop mm se 2guru.controls
 Modules.Global. inneh?ller f?rdig funktioner kolla d?r!  
 
 admin program dessa program k?rs ej INNAN att du vet vad de g?r!!!
 ADMDYNBROWSE.W  fisar alla tabeller kan uppdatera tabort l?gga till visa poster
 
 Avsluta anv?ndare och f?retag Ny anv?ndare nytt f?retag
 1. S?tt stoppdatum p? anv?ndaren i sekretess och informera fakturpers om f?r?ndringen. Obs ska m?nadspriset r?knas om?
    Datumet ska vara sista den f?r k?ra, faktruraavdelningen ska ha sista faktureringsdatum
    N?r stoppdatum intr?ffar, ta bort anv?ndaren
 
 2  Avsluta f?retag genomf?r punkt 1 ovan! G?r att g?ra p? f?rsta anv?ndare i sekretess sen g?r det att k?ra via  ADMDYNBROWSE.W 
    S?tt valdbtemp.DBCACHE = "????-mm-dd" i f?retagets VALDBbxxx.I  
    D? syns det n?r databasen ska stoppas i V?lj databas. Det sker inga backupper fr?n stoppdatum
    Ta bort VALDBbxxx.I fr?n VALDBALL.I och WC-CHECK.P  
    k?r XCOMPOMNYKUDKOMPILERA.P
    flytta alla program till server
    ta bort db fr?n server rensa backupper utom den sista.
 
 3. Ny anv?ndare/ nytt f?retag 
    Skapa ny databas, l?gg IN ny sekretess
    
    Skapa VALDBbxxx.I fil kolla portnr f?retag gforetag mm
    L?gg IN VALDBbxxx.I i VALDBALL.I och r?tt under fil.
    L?gg IN VALDBbxxx.I WC-CHECK.P
    L?gg IN ev logga i LOGGORIN.P. 
    L?gg IN logga i BILDEr-mappen
    k?r XCOMPOMNYKUDKOMPILERA.P
    flytta alla program till server
    Starta db p? r?tt server 
    L?gg IN anv?ndare
    Skapa ny VERSION eller installera anv?ndare och l?gg dit WC-CHECK.P och logga manuelt     
    informera fakturpers om f?r?ndringen. Obs ska m?nadspriset r?knas om?
 
 
 
 ALLMANaonrPagaende.P  ?ndra aonr SPECIAL F?R AONR
 ALLMANBYTaonr.P  SPECIAL F?R AONR  
 ALLMaonrbort.P tar bort aonr helt fr?n db MEDDELANDE alla tillh?rande tabeller 
 
 AnonymAONR.p g?r personer anonyma p? det som g?ller aonr
 AnonymPkod.p  g?r personer anonyma p? det som g?ller ?rigt som tidskrivning mm se annan dokumenta tion 
 ALLMANBYT.P BYTER P? UNICA NYKLAR I ALLA TABELLER I DATABASEN F?R ANGIVET V?RDE TILL ANGIVET V?RDE F?R ANGIVET F?LT I DATABASEN
 
 AONRNAMNKOLL.P kollar indat s? det ej har otil?tna tecken
 EPOSTKOLL.P  kollar indat s? det ej har otil?tna tecken
 
EXTRATABHMT.P N?R KOPPLINGAR MELLAN TABELLER BEH?VS
EXTRADATAHMT.P N?R EN TABELL SAKNAR F?LT
EXTRADATAHMTDS.P  EXTRADATA VIA PRODATASET 
EXTRADATSPEC.P SPECIAL F?R AONR!
STARTADMDB.w f?r att hantera databaser mm se STARTADMDB.w
STARTADMMEDSKAP.P skapar MEDDELANDE till flera databaser
STARTADMVisaAvSekApp.P visar GURUKOLL EXTRADATA
GURUSUPPORTANV.W F?R ATT L?GGA IN GURUSUPPORT USER 
XCOMPALLTSUPER.p KOMPILERAR NY GURU VERSION
XASCIIBOKSTAV.w F?R KRYPTERING TEXT
XCOMPOMNYKUDKOMPILERA.P DET SOM M?STE KOMPILERAS VID NY KUND G? IGENOM VARJE PROGRAM SOM KOMPILERAS S? DET BLIR R?TT 
XCOMPWEBcontrols.p F?R ATT KOMILERA PROGRAM MED L?GSTA EXCEL VERSION
XLABEL.W F?R ATT L?GGA UPP SEKRETESS HOS KUND

STARTload_df.p F?R ATT L?GGA TILL TABELLER OCGH F?LT I DATABASER OBS!
?NDRING AVAILABLEDESSA PROGRAM KR?VER https://knowledgebase.progress.com/articles/Knowledge/P147079
COMPILE XCODE
xcode [ -k key ] -d directory [ files] [ - ]  DVS INGEN KOMPILERING UTAN KRYPTERING 


XCOPYTABLE.W  KOPIERA TABELLER OK?ND STATUS
XCOPYTABLE2.W  KOPIERA TABELLER OK?ND STATUS
XJAMFORTABLE.W j?mf?rtabeller i olika databaser OK?ND STATUS

/*Anders Olsson Elpool i Ume? AB  31 okt 2022 13:48:14*/ 
KOPPLA ELLER KOPIERA KALKYL
    KOPKALK.W k?rs fr?n AONR OCH PLAN. I PLAN KAN MANAD F?RA ?VER PROJEKTET TILL AONR.
    /*vartifran = 2 AONR koppla
vartifran = 3 PLAN koppla
vartifran = 4 AONR kopiera
vartifran = 5 PLAN kopiera
vartifran = 99 PLAN KALK TILL AONR KALK. KOPPLA AONR TILL PLAN M?STE VARAAKTIVT
 
*/

k?r via MTRLELPOV.W  

RUN DELALLBER.P tar bort alla beredningar inklusive kopplingar
RUN DELALLMARK.P tar bort alla markv?rderingar inklusive kopplingar
RUN DELALLDEP.P tar bort alla dep?er  inklusive kopplingar
RUN DELALLKALK.P tar bort alla KALKYLER  inklusive kopplingar
    
    KOPPLA FR?N KOPKALK.W BARA AKTIVA KALKYLER KOPPLINGEN SKER I KALKRUTINK1.P
    KOPIERA FR?N KOPKALK.W KOPIERINGEN SKER I copycat_UI S?TTS FN KALKHUV.AKTIV = TRUE.
    KOPIERA FR?N KALKYLDB.cls   inklusive FRIKalkyl kopieraFRI_UI
                              annars  kopierakalkyl_UI  copycat_UI
                              HAR KALKYLEN AONR F?R KALKYLEN SAMMA KALKHUV.AKTIV SOM STARTKALKYLEN
                              SAKBNAS AONR BLIR KALKHUV.AKTIV = TRUE   
                              
 autoprogram
 
 AUTO5SPAR.P  k?rs ej
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
  
  AUTOAPPSTARTVATTPRO.P   uppdaterar beredningar k?rs separat fr?n ?vriga nattk?rningar.
  AUTOVATTPRO.P
     RUN GRANAHLS.P (INPUT 1, INPUT 1).
  AUTOVATTDP.P
       RUN GRANAHLS.P (INPUT 1, INPUT 1).
  
  
  AUTOBODEN.P k?rs ej ?      
  AUTOELPSALJ.p  elpools s?lj databas   
  AUTOELTEL.P  k?rs ej ?
  AUTOFORS.P  
     RUN NAPRBER.P.  /* uppdatera pris i beredning om katalog uppdaterad*/
 AUTOLAPP.P    k?rs ej ?
 AUTOONENO.p 
 AUTOREJL.P
 AUTOUMEA.P
    RUN AUTOMARKUM.P.      
 AUTOWWW2EGNAIP.p
        IF valdbtemp.DBNAMN = "SUNDNAT" THEN RUN AUTOSU9BERGET.p (INPUT valdbtemp.FORETAG ).
        IF valdbtemp.DBNAMN = "KRIN" THEN RUN NAPRBER.P.  /* uppdatera pris i beredning om katalog uppdaterad*/
        IF valdbtemp.DBNAMN = "UMBR" THEN RUN AUTOMARKUM.P.  /*skicka meddelande valda datum*/
AUTOWWW2EGNAIPVAL.p   k?rs ej      


NYBEREU.W                            
 franvart = varifran = 1 NY/ELLER ?NDRAD DIREKT FR?N BEREDNINGSMENYN import av beredning
franvart = varifran = 3 NY BERE FR?N AONRHUVUD
franvart = varifran = 2 ?NDRAD BERE FR?N AONRHUVUD 
vart = NYA eller AND DVS NY BEREDNING ELLER ?NDRA HUVUDET P? BEFINTLIG
lnamnex ?R LEVERANT?R VALD FR?M IMPORT
FR?N 
EXPIMPBERDS.W
   FR?N BERMENY.P
   IMPORT FR?N ANNAN BEREDNING
   vart = "NYA".
   lnamnex = LEVERANT?RENS NAMN I KLARTEXT 
   RUN NYBEREU.W (INPUT 1,INPUT lnamnex ).
   IMPORT FR?N AVCAD TYPDATOR = 3 = FR?N AVCAD
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
      ?NDRA    
          franvart = 2    
          RUN menyberval_UI IN Guru.SharedVariable:btnberh (INPUT bervart,INPUT 2, INPUT aonrvar, INPUT delnrvar, INPUT TABLE berintemp,INPUT-OUTPUT inkopvart).
          andra_UI   
          vart = "AND".
          andra_UI  
          RUN NYBEREU.W (INPUT franvart,INPUT "").
      IMPORT
      vart = "NYA". 
      VIA BERMENY.P  EXPIMPBERDS.W   
   
