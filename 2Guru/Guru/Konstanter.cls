
 
 /*------------------------------------------------------------------------
    File        : Konstanter
    Purpose     : 
    Syntax      : Guru.Konstanter:globGuruversion
    Description : 
    Author(s)   : elpkl
    Created     : Tue Feb 08 16:27:30 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS Guru.Konstanter FINAL: 
   /*START*/
   /*för att man ska kunna komma åt tabeller över allt.*/
   DEFINE PUBLIC STATIC PROPERTY startGlobalFakeRoot      AS Guru.FakeRoot   NO-UNDO  /*sätts i REGLERFORLOSEN.P  bara på klient*/ 
      PUBLIC GET.    PUBLIC SET.
     
   
   DEFINE PUBLIC STATIC PROPERTY globalroot      AS Guru.Root   NO-UNDO               /* sätts i ROOT.CLS för varje anrop till .Net moduler borde vara*/ 
      PUBLIC GET.    PUBLIC SET.                                                      /*Guru.GlobalaVariabler bara på klient*/
                                                                                      /*globalroot för dom som inte har Root som input*/                        
   
  /*TEMPTABELLER  SÄTTS I FakeRoot.cls bara på klient  GLOBALSTART.I*/ 
   DEFINE PUBLIC STATIC PROPERTY AnvandareTTh AS HANDLE NO-UNDO
      PUBLIC GET.    PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY PersonalTTh AS HANDLE NO-UNDO
      PUBLIC GET.    PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY OmradeTTh AS HANDLE NO-UNDO
      PUBLIC GET.    PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY BorttagnaOmradeTTh AS HANDLE NO-UNDO
      PUBLIC GET.    PUBLIC SET.   
   DEFINE PUBLIC STATIC PROPERTY BestKundTTh AS HANDLE NO-UNDO
      PUBLIC GET.    PUBLIC SET.   
   DEFINE PUBLIC STATIC PROPERTY JurPersTTh AS HANDLE NO-UNDO
      PUBLIC GET.    PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY JurAvdTTh AS HANDLE NO-UNDO
      PUBLIC GET.    PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY LeverantorTTh AS HANDLE NO-UNDO
      PUBLIC GET.    PUBLIC SET.
  
  /*TEMPTABELLER  SÄTTS I FakeRoot.cls*/ 
  /*konstanter olika för server och klient*/ 
  
      
   DEFINE PUBLIC STATIC PROPERTY globalhkeyvar AS CHARACTER /*startclass.p klient*/
      PUBLIC GET.    PUBLIC SET.
      
  /*PROVAG.I*/    
   DEFINE PUBLIC STATIC PROPERTY gurubilder AS CHARACTER /*startclass.p klient*/
      PUBLIC GET.    PUBLIC SET.   
   DEFINE PUBLIC STATIC PROPERTY dlcvar AS CHARACTER  /*startclass.p klient  VAR DLC\BIN   sätts i PROVAG.I*/
      PUBLIC GET.    PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY wtidvar AS CHARACTER  /*startclass.p klient  GURU\WTID eller guru\ctid peroende för klient val */
      PUBLIC GET.    PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY guruvar AS CHARACTER /*startclass.p klient pekar på guru.*/
      PUBLIC GET.    PUBLIC SET.    
  /*PROVAG.I*/
  
   
   /*konstanter olika för server och klient*/   
    
   /*GLOBVAR2.I*/
   DEFINE PUBLIC STATIC PROPERTY globanv AS CHARACTER  /* REGLERFORLOSEN.P klient och server SPRINGKONTROLL.P LOSENKOLLWEB.P*/
      PUBLIC GET.      PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY globanvbyt AS CHARACTER /* GuruClasserStart.p bara klient för att byta användare*/
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY globanvnt AS CHARACTER /* REGLERFORLOSEN.P bara klient datorns användare*/
      PUBLIC GET. PUBLIC SET.   
   DEFINE PUBLIC STATIC PROPERTY datornamn AS CHARACTER  /* REGLERFORLOSEN.P bara klient datornsNAMN*/
      PUBLIC GET. PUBLIC SET.
   
   DEFINE PUBLIC STATIC PROPERTY globanvavdnr AS INTEGER  /* REGLERFORLOSEN.P bara klient  globanvavdnr = ANVANDARE.AVDELNINGNR*/
      PUBLIC GET. PUBLIC SET.   
   DEFINE PUBLIC STATIC PROPERTY globanvpkod AS CHARACTER  /* REGLERFORLOSEN.P bara klient  globanvpkod = ANVANDARE.PERSONALKOD*/
      PUBLIC GET. PUBLIC SET.   
   DEFINE PUBLIC STATIC PROPERTY globallm AS LOGICAL    /* REGLERFORLOSEN.P bara klient IF Guru.Konstanter:globanv = "ALLMÄN" THEN globallm = TRUE.*/
      PUBLIC GET. PUBLIC SET.                           /*   ELSE IF Guru.Konstanter:globanv = "ALLMÄN2" THEN globallm = TRUE.    ELSE globallm = FALSE.*/
         
   DEFINE PUBLIC STATIC PROPERTY globniv AS INTEGER INITIAL ? /* REGLERFORLOSEN.P  klient   ANVANDARE.AV-LEVEL server  LOSENKOLLWEB.P*/ 
      PUBLIC GET. PUBLIC SET.   
   DEFINE PUBLIC STATIC PROPERTY globallpers AS LOGICAL  /* REGLERFORLOSEN.P bara klient  globallpers = ANVANDARE.ALLPERS */
      PUBLIC GET. PUBLIC SET.   
   DEFINE PUBLIC STATIC PROPERTY globallao AS LOGICAL /* REGLERFORLOSEN.P bara klient Guru.Konstanter:globallao = ANVANDARE.ALLAONR*/
      PUBLIC GET. PUBLIC SET.  
   DEFINE PUBLIC STATIC PROPERTY globavd AS INTEGER /* REGLERFORLOSEN.P bara klient AVDELNING.AVDELNINGNR*/
      PUBLIC GET. PUBLIC SET.   
   DEFINE PUBLIC STATIC PROPERTY globomr AS CHARACTER /* REGLERFORLOSEN.P bara klient  PERSONALTAB.OMRADE server  LOSENKOLLWEB.P*/
       PUBLIC GET. PUBLIC SET.   
   DEFINE PUBLIC STATIC PROPERTY globjid AS CHARACTER /* REGLERFORLOSEN.P bara klient  JURPERS.JUDID */
      PUBLIC GET. PUBLIC SET.  
   DEFINE PUBLIC STATIC PROPERTY globpersnamn AS CHARACTER /* REGLERFORLOSEN.P bara klient  ANVANDARE.AV-NAMN  */ 
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY globforetag AS CHARACTER /* Guru.Konstanter:AppSpringSet[2] = startclass.p klient  server  LOSENKOLLWEB.P VIA REGLERFORLOSEN.P*/
      PUBLIC GET. PUBLIC SET. 
   DEFINE PUBLIC STATIC PROPERTY GForetag AS CHARACTER  /*  Guru.Konstanter:AppSpringSet[1]  = Spring.p  klient server  LOSENKOLLWEB.P */     
      PUBLIC GET. PUBLIC SET.  
   DEFINE PUBLIC STATIC PROPERTY globstorb AS INTEGER   /* REGLERFORLOSEN.P bara klient  globstorb = ANVANDARE.SIDL  bredd maxfönster samma som globsidl*/ 
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY globstorh AS INTEGER   /* REGLERFORLOSEN.P bara klient  globstorh = ANVANDARE.SIDS  höjd*/
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY globDefaultstorb AS INTEGER  /* REGLERFORLOSEN.P bara klient default normalfönster*/
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY globDefaultstorh AS INTEGER /* REGLERFORLOSEN.P bara klient*/
      PUBLIC GET. PUBLIC SET.
   /*
   DEFINE PUBLIC STATIC PROPERTY globsidl AS INTEGER     /* REGLERFORLOSEN.P bara klient  default maxfönster BREDD*/ 
      PUBLIC GET. PUBLIC SET.  
   DEFINE PUBLIC STATIC PROPERTY globsids AS INTEGER  /* REGLERFORLOSEN.P bara klient default maxfönster HÖJD  EJSKRIVARE*/
      PUBLIC GET. PUBLIC SET.  
  */
   DEFINE PUBLIC STATIC PROPERTY globradbrytch AS CHARACTER  /* REGLERFORLOSEN.P bara klient chr(10) */
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY globnetprissortvar AS INTEGER  /* REGLERFORLOSEN.P bara klient*/
      PUBLIC GET. PUBLIC SET.                                  /*"kund DESCENDING by enr"*/
   
   DEFINE PUBLIC STATIC PROPERTY globGuruversion AS INTEGER  /* läses från Registry  WEBVERSION.P bara klient*/
      PUBLIC GET. PUBLIC SET.                                   
   
   DEFINE PUBLIC STATIC PROPERTY AppSpringSet AS CHARACTER {AppServerInfoExtent.i} /*klient spring.p springstart.p och server AppSpringDbCon.P */
      PUBLIC GET.     PUBLIC SET.
      {AppSpringSetInfo.I} 
   
   DEFINE PUBLIC STATIC PROPERTY varforetypval AS INTEGER EXTENT 100     
      PUBLIC GET.     PUBLIC SET.       /* LADDAS I *AppSpringDbCon.P FÖR SERVER */
   DEFINE PUBLIC STATIC PROPERTY varforetypchar AS CHARACTER EXTENT 100
      PUBLIC GET.     PUBLIC SET.       /* LADDAS I *AppSpringDbCon.P FÖR SERVER */
   DEFINE PUBLIC STATIC PROPERTY globnystart AS LOGICAL   /*FÖR ATT KOLLA VISSA FONTER*/
      PUBLIC GET.     PUBLIC SET. 
   DEFINE PUBLIC STATIC PROPERTY alltidmax AS LOGICAL  /*REGLERFORLOSEN.P  BARA PÅ KLIENTV */
      PUBLIC GET.     PUBLIC SET. 
   
   /*GLOBVAR.I*/
   
   DEFINE PUBLIC STATIC PROPERTY conappvar  AS CHARACTER  /*ska ej sättas på server*/
      PUBLIC GET.     PUBLIC SET. 
   DEFINE PUBLIC STATIC PROPERTY appcon  AS LOGICAL   /*ska ej sättas på server*/
      PUBLIC GET.     PUBLIC SET. 
   DEFINE PUBLIC STATIC PROPERTY apphand AS HANDLE   /*ska ej sättas på server*/
      PUBLIC GET.     PUBLIC SET. 
   DEFINE PUBLIC STATIC PROPERTY appfel  AS LOGICAL /*BORDE VARA ENRVAL VARIABEL*/
      PUBLIC GET.     PUBLIC SET. 
   DEFINE PUBLIC STATIC PROPERTY MultiStarth AS HANDLE
      PUBLIC GET.     PUBLIC SET. 
   DEFINE PUBLIC STATIC PROPERTY hpApi AS HANDLE
      PUBLIC GET.     PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY hpWinFunc AS HANDLE
      PUBLIC GET.     PUBLIC SET.       
      
   /*REGLERFORLOSEN.P kör fakeroot på klient LaddakonstanterApp.P på server
   om en ny globalkonstan  gxxxx skapas 
   sÖK PÅ:  nya globala konstanter i TEMPTABELLERAPP.p då skapas alla poser automatsikt för alla databaser som körs
    om språk  FakeRoot  TranslateConstantsSet.I VIA Fakeroot   SERVER LaddakonstanterApp.P VIA Fakeroot och TranslateConstantsSet.I
    ändrig av ett enskilt värde görs via Språkverktyg i Register det krävs urloggning från systemet för att det ska visas. 
    */
   DEFINE PUBLIC STATIC PROPERTY gavdl AS CHARACTER /*AVDELNING LÅNG*/
      PUBLIC GET. PUBLIC SET. 
   DEFINE PUBLIC STATIC PROPERTY gavdk AS CHARACTER /*AVDELNING KORT*/ 
      PUBLIC GET. PUBLIC SET.     
   DEFINE PUBLIC STATIC PROPERTY gomrl AS CHARACTER /*OMRÅDE LÅNG*/ 
      PUBLIC GET. PUBLIC SET.  
   DEFINE PUBLIC STATIC PROPERTY gomrk AS CHARACTER     /*OMRÅDE KORT*/ 
      PUBLIC GET. PUBLIC SET. 
   DEFINE PUBLIC STATIC PROPERTY gaol AS CHARACTER /*AONR LÅNG*/ 
      PUBLIC GET. PUBLIC SET.   
   DEFINE PUBLIC STATIC PROPERTY gaok AS CHARACTER  /*AONR KORT*/  
      PUBLIC GET. PUBLIC SET.       
   DEFINE PUBLIC STATIC PROPERTY gdelnrl AS CHARACTER  INITIAL "Delnr" /*delnr LÅNG*/ 
      PUBLIC GET. PUBLIC SET.   
   DEFINE PUBLIC STATIC PROPERTY gdelnrk AS CHARACTER INITIAL "Delnr" /*delnr KORT*/  
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY gutbytk AS CHARACTER INITIAL "Ersättningslista" /* KORT*/  
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY gpll AS CHARACTER /*PLANNR LÅNG*/
      PUBLIC GET. PUBLIC SET.  
   DEFINE PUBLIC STATIC PROPERTY gplk AS CHARACTER /*PLANNR KORT*/
      PUBLIC GET. PUBLIC SET.     
   DEFINE PUBLIC STATIC PROPERTY genl AS CHARACTER /*E-NUMMER LÅNG*/ 
      PUBLIC GET. PUBLIC SET. 
   DEFINE PUBLIC STATIC PROPERTY genk AS CHARACTER /*E-NUMMER KORT*/
      PUBLIC GET. PUBLIC SET.  
   DEFINE PUBLIC STATIC PROPERTY gjul AS CHARACTER /*JURIDISK LÅNG*/ 
      PUBLIC GET. PUBLIC SET. 
   DEFINE PUBLIC STATIC PROPERTY gjuk AS CHARACTER /*JURIDISK KORT*/
      PUBLIC GET. PUBLIC SET.  
   DEFINE PUBLIC STATIC PROPERTY gfastl AS CHARACTER /*FASTA LÅNG*/ 
      PUBLIC GET. PUBLIC SET. 
   DEFINE PUBLIC STATIC PROPERTY gfastk AS CHARACTER  /*FASTA KORT*/
      PUBLIC GET. PUBLIC SET.    
   DEFINE PUBLIC STATIC PROPERTY gtilll AS CHARACTER /*TILLFÄLIGA LÅNG*/
      PUBLIC GET. PUBLIC SET.  
   DEFINE PUBLIC STATIC PROPERTY gtillk AS CHARACTER  /*TILLFÄLIGA KORT*/
      PUBLIC GET. PUBLIC SET.    
   DEFINE PUBLIC STATIC PROPERTY gberel AS CHARACTER /*BEREDARE LÅNG*/ 
      PUBLIC GET. PUBLIC SET. 
   DEFINE PUBLIC STATIC PROPERTY gberek AS CHARACTER /*BEREDARE KORT*/
      PUBLIC GET. PUBLIC SET.     
   DEFINE PUBLIC STATIC PROPERTY gprojl AS CHARACTER  /*PROJEKTÖR LÅNG*/ 
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY gprojk AS CHARACTER /*PROJEKTÖR KORT*/
      PUBLIC GET. PUBLIC SET.     
   DEFINE PUBLIC STATIC PROPERTY garbal AS CHARACTER /*ARBETSANSVARIG LÅNG*/
      PUBLIC GET. PUBLIC SET.  
   DEFINE PUBLIC STATIC PROPERTY garbak AS CHARACTER  /*ARBETSANSVARIG KORT*/
      PUBLIC GET. PUBLIC SET.    
   DEFINE PUBLIC STATIC PROPERTY gutfk AS CHARACTER /*Utförande KORT*/
      PUBLIC GET. PUBLIC SET.     
   DEFINE PUBLIC STATIC PROPERTY gutfl AS CHARACTER /*Utförande LÅNG*/ 
      PUBLIC GET. PUBLIC SET. 
   DEFINE PUBLIC STATIC PROPERTY gbestk AS CHARACTER  /*Beställare/Kund KORT*/
      PUBLIC GET. PUBLIC SET.    
   DEFINE PUBLIC STATIC PROPERTY gbestl AS CHARACTER /*Beställare/Kund LÅNG*/ 
      PUBLIC GET. PUBLIC SET. 
   DEFINE PUBLIC STATIC PROPERTY gdebk AS CHARACTER  /*Debiterigstyp KORT*/
      PUBLIC GET. PUBLIC SET.    
   DEFINE PUBLIC STATIC PROPERTY gdebl AS CHARACTER /*Debiterigstyp LÅNG*/ 
      PUBLIC GET. PUBLIC SET.  
   DEFINE PUBLIC STATIC PROPERTY gtidlk AS CHARACTER /*tidläge KORT*/
      PUBLIC GET. PUBLIC SET.     
   DEFINE PUBLIC STATIC PROPERTY gtidll AS CHARACTER /*tidläge LÅNG*/ 
      PUBLIC GET. PUBLIC SET.  
   DEFINE PUBLIC STATIC PROPERTY gutfardk AS CHARACTER  /*utfärdat av KORT*/
      PUBLIC GET. PUBLIC SET.    
   DEFINE PUBLIC STATIC PROPERTY gutfardl AS CHARACTER /*utfärdat av LÅNG*/ 
      PUBLIC GET. PUBLIC SET.  
   DEFINE PUBLIC STATIC PROPERTY grefbefk AS CHARACTER /*Ref.nr beställare KORT*/
      PUBLIC GET. PUBLIC SET.     
   DEFINE PUBLIC STATIC PROPERTY grefbefl AS CHARACTER /*Ref.nr beställare  LÅNG*/ 
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY gpriok AS CHARACTER NO-UNDO     /*Prioritet KORT*/
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY gpriol AS CHARACTER NO-UNDO  /*Prioritet  LÅNG*/
      PUBLIC GET. PUBLIC SET. 
   DEFINE PUBLIC STATIC PROPERTY gartk AS CHARACTER NO-UNDO     /*Arbetsart KORT*/
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY gartl AS CHARACTER NO-UNDO  /*Arbetsart  LÅNG*/
      PUBLIC GET. PUBLIC SET. 
   DEFINE PUBLIC STATIC PROPERTY gaonamnk AS CHARACTER NO-UNDO     /*Ort/Benämning aonr KORT*/
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY gaonamnl AS CHARACTER NO-UNDO  /*Ort/Benämning aonr  LÅNG*/
      PUBLIC GET. PUBLIC SET. 
   DEFINE PUBLIC STATIC PROPERTY globsprak AS INTEGER NO-UNDO  /*Ort/Benämning aonr  LÅNG*/
      PUBLIC GET. PUBLIC SET.
   /*EGENBVAR.I  om språk  FakeRoot  TranslateConstantsSet.I   SERVER LaddakonstanterApp.P*/   
      /*alla sekvar laddas i REGLERFORLOSEN.P  app  LOSENKOLLWEB.P*/
   DEFINE PUBLIC STATIC PROPERTY hoppsekvar AS LOGICAL EXTENT 50 NO-UNDO    /*alla sekvar laddas i client REGLERFORLOSEN.P  SERVER  LOSENKOLLWEB.P SE LOSENREGLERSEK.I*/
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY aonrsekvar AS LOGICAL EXTENT 50 NO-UNDO   /*alla sekvar laddas i client REGLERFORLOSEN.P  SERVER  LOSENKOLLWEB.P*/
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY bulasekvar AS LOGICAL EXTENT 50 NO-UNDO
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY beresekvar AS LOGICAL EXTENT 50 NO-UNDO
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY faktsekvar AS LOGICAL EXTENT 50 NO-UNDO
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY kalk2sekvar AS LOGICAL EXTENT 50 NO-UNDO
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY arendesek AS LOGICAL EXTENT 50 NO-UNDO
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY mtrlsekvar AS LOGICAL EXTENT 50 NO-UNDO
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY persekvar AS LOGICAL EXTENT 50 NO-UNDO
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY plansekvar AS LOGICAL EXTENT 50 NO-UNDO
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY regsekvar AS LOGICAL EXTENT 50 NO-UNDO
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY tadmsekvar AS LOGICAL EXTENT 50 NO-UNDO
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY tidasekvar AS LOGICAL EXTENT 50 NO-UNDO
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY tidbsekvar AS LOGICAL EXTENT 50 NO-UNDO
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY tidosekvar AS LOGICAL EXTENT 50 NO-UNDO
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY tidrsekvar AS LOGICAL EXTENT 50 NO-UNDO
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY tidsekvar AS LOGICAL EXTENT 50 NO-UNDO
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY tidssekvar AS LOGICAL EXTENT 50 NO-UNDO
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY tidtsekvar AS LOGICAL EXTENT 50 NO-UNDO
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY storsekvar AS LOGICAL EXTENT 50 NO-UNDO
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY MtrlClasserStart AS HANDLE NO-UNDO  /*bara klientsätts i detta program */  
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY HuvudLeverantor     AS CHARACTER NO-UNDO  /*företagets huvudlev CLIENT fakeroot OCH SERVER TEMPTABELLERAPP.p*/
      PUBLIC GET.    PUBLIC SET.  
   DEFINE PUBLIC STATIC PROPERTY  HuvudLeverantorName AS CHARACTER NO-UNDO
      PUBLIC GET.    PUBLIC SET.
   /*SEKRETESS*/
    DEFINE PUBLIC STATIC PROPERTY  Salt AS CHARACTER  NO-UNDO
      PUBLIC GET.    PUBLIC SET.
    DEFINE PUBLIC STATIC PROPERTY  Rounds AS INTEGER NO-UNDO  
      PUBLIC GET.    PUBLIC SET.
    DEFINE PUBLIC STATIC PROPERTY  AlgoR AS CHARACTER NO-UNDO 
      PUBLIC GET.    PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY  EncAlgoR AS CHARACTER NO-UNDO 
      PUBLIC GET.    PUBLIC SET.
     




      
   METHOD PUBLIC STATIC VOID MellanRumKnappar(mellan AS DECIMAL):
      Guru.GlobalaVariabler:MellanRumKnappar = mellan.
   END METHOD.
   
   METHOD PUBLIC STATIC  VOID MtrlHelpStart():
      IF NOT VALID-HANDLE(MtrlClasserStart) THEN DO:
         RUN Modules\Materiel\MtrlHelpStart.p PERSISTENT SET MtrlClasserStart.
      END.  
      
   END METHOD.
  
   METHOD PUBLIC STATIC VOID PlaceraKnapparLodratt(butthnd AS HANDLE):
      IF  Guru.GlobalaVariabler:MellanRumKnappar = 0 THEN  Guru.GlobalaVariabler:MellanRumKnappar = 0.1.
      IF butthnd:HIDDEN = FALSE THEN DO:
         butthnd:ROW = Guru.GlobalaVariabler:StartRadForKnappar.
         Guru.GlobalaVariabler:StartRadForKnappar = Guru.GlobalaVariabler:StartRadForKnappar + Guru.GlobalaVariabler:MellanRumKnappar + butthnd:HEIGHT-CHARS.
         LabelFlytt(butthnd). 
      END.   
   END METHOD.  
   METHOD PUBLIC STATIC VOID PlaceraKnapparVagratt(butthnd AS HANDLE, uppda AS LOGICAL):
      IF  Guru.GlobalaVariabler:MellanRumKnappar = 0 THEN  Guru.GlobalaVariabler:MellanRumKnappar = 0.1.
      IF butthnd:HIDDEN = FALSE THEN DO:
         butthnd:COLUMN = Guru.GlobalaVariabler:StartKolumnForKnappar.
         IF uppda = TRUE THEN Guru.GlobalaVariabler:StartKolumnForKnappar = Guru.GlobalaVariabler:StartKolumnForKnappar + Guru.GlobalaVariabler:MellanRumKnappar + butthnd:WIDTH-CHARS.
         LabelFlytt(butthnd). 
      END.   
   END METHOD.
   METHOD PUBLIC STATIC VOID PlaceraKnapparVagrattFranHoger(butthnd AS HANDLE):
      IF  Guru.GlobalaVariabler:MellanRumKnappar = 0 THEN  Guru.GlobalaVariabler:MellanRumKnappar = 0.1.
      IF butthnd:HIDDEN = FALSE THEN DO:
         
         butthnd:COLUMN = Guru.GlobalaVariabler:StartKolumnForKnappar - butthnd:WIDTH-CHARS .
        
         Guru.GlobalaVariabler:StartKolumnForKnappar = butthnd:COLUMN - Guru.GlobalaVariabler:MellanRumKnappar.
         LabelFlytt(butthnd). 
      END. 
   END METHOD.
   METHOD PUBLIC STATIC VOID LabelFlytt(butthnd AS HANDLE):
      IF butthnd:TYPE = "COMBO-BOX" OR butthnd:TYPE = "EDITOR" OR
      butthnd:TYPE = "RADIO-SET" OR butthnd:TYPE = "SELECTION-LIST" OR butthnd:TYPE = "FILL-IN" OR
      butthnd:TYPE = "SLIDER" OR butthnd:TYPE = "TEXT" THEN.
      ELSE RETURN. 
      IF butthnd:SIDE-LABEL-HANDLE = ? THEN RETURN.
      /*
      butthnd:SIDE-LABEL-HANDLE:X = butthnd:X - butthnd:SIDE-LABEL-HANDLE:WIDTH-PIXELS.
      butthnd:SIDE-LABEL-HANDLE:Y = butthnd:Y.
      */
      butthnd:LABEL = TRIM(butthnd:LABEL). 
      butthnd:SIDE-LABEL-HANDLE:COLUMN = butthnd:COLUMN - butthnd:SIDE-LABEL-HANDLE:WIDTH-CHARS.
      butthnd:SIDE-LABEL-HANDLE:Y = butthnd:Y.
   END METHOD.
   METHOD PUBLIC STATIC VOID LabelFlyttExtra(butthnd AS HANDLE):
      IF butthnd:TYPE = "COMBO-BOX" OR butthnd:TYPE = "EDITOR" OR
      butthnd:TYPE = "RADIO-SET" OR butthnd:TYPE = "SELECTION-LIST" OR butthnd:TYPE = "FILL-IN" OR
      butthnd:TYPE = "SLIDER" OR butthnd:TYPE = "TEXT" THEN.
      ELSE RETURN. 
      IF butthnd:SIDE-LABEL-HANDLE = ? THEN RETURN.
      
      butthnd:LABEL = TRIM(butthnd:LABEL). 
      butthnd:SIDE-LABEL-HANDLE:COLUMN = butthnd:COLUMN.
      butthnd:SIDE-LABEL-HANDLE:Y = butthnd:Y. 
      butthnd:COLUMN = butthnd:COLUMN + butthnd:SIDE-LABEL-HANDLE:WIDTH-CHARS.
      
   END METHOD.
   METHOD PUBLIC STATIC VOID BtnOkAvbFlytt():
      IF NOT VALID-HANDLE(Guru.GlobalaVariabler:BtnOkh) THEN RETURN.
      IF NOT VALID-HANDLE(Guru.GlobalaVariabler:BtnAvbh) THEN RETURN. 
      IF  Guru.GlobalaVariabler:BtnOkh = ? THEN RETURN. 
      IF  Guru.GlobalaVariabler:BtnAvbh = ? THEN RETURN.
      IF  Guru.GlobalaVariabler:BtnOkh:ROW NE Guru.GlobalaVariabler:BtnAvbh:ROW THEN RETURN.
      IF  Guru.GlobalaVariabler:MellanRumKnappar = 0 THEN  Guru.GlobalaVariabler:MellanRumKnappar = 0.1.
      IF Guru.GlobalaVariabler:windowsizeh NE ? THEN DO:
         Guru.GlobalaVariabler:BtnAvbh:COLUMN = Guru.GlobalaVariabler:windowsizeh:WIDTH-CHARS - (Guru.GlobalaVariabler:BtnAvbh:WIDTH-CHARS + Guru.GlobalaVariabler:MellanRumKnappar) NO-ERROR.
      END.   
     
      Guru.GlobalaVariabler:BtnOkh:COLUMN = Guru.GlobalaVariabler:BtnAvbh:COLUMN - (Guru.GlobalaVariabler:BtnAvbh:WIDTH-CHARS + Guru.GlobalaVariabler:MellanRumKnappar) NO-ERROR. 
       
   END METHOD.
   
   
   METHOD PUBLIC STATIC VOID Taborder(my1hand AS HANDLE,butthnd AS HANDLE):
      butthnd:MOVE-AFTER-TAB-ITEM(my1hand).
   END METHOD.    
   METHOD PUBLIC STATIC CHARACTER InternNr(nrvar AS CHARACTER ):
      RETURN nrvar .
   END METHOD.
   /*
   METHOD PUBLIC STATIC VOID SaltAndHashStart():
      SaltAndHash = NEW Guru.SaltAndHashManager().
   END METHOD.
   */
   /*Anders Olsson Elpool i Umeå AB  22 nov 2022 14:35:13 
   SaltData körs från 
   STARTMULTI.P
   LOSENKOLLWEB.p
   STARTADMDB.w 
   */
   METHOD PUBLIC STATIC VOID SaltData():
      {SaltData.I}
   END METHOD.
   METHOD PUBLIC STATIC CHARACTER SaltRetur(saltpswd  AS CHARACTER) :
   
     {SaltAndHash.I}
     {SaltReturn.i}
     
   END METHOD.
   METHOD PUBLIC STATIC VOID CreateVallistaDynTable(INPUT vallistabufftth AS HANDLE):
      CREATE TEMP-TABLE Guru.GlobalaVariabler:ttHandleVallistaDyn.
      Guru.GlobalaVariabler:ttHandleVallistaDyn:CREATE-LIKE(vallistabufftth).
      Guru.GlobalaVariabler:ttHandleVallistaDyn:TEMP-TABLE-PREPARE("VL" + vallistabufftth:TABLE).
      Guru.GlobalaVariabler:ttBuffHandleVallistaDyn = Guru.GlobalaVariabler:ttHandleVallistaDyn:DEFAULT-BUFFER-HANDLE. 
      CREATE TEMP-TABLE Guru.GlobalaVariabler:ttHandleValdaDyn.
      Guru.GlobalaVariabler:ttHandleValdaDyn:CREATE-LIKE(vallistabufftth).
      Guru.GlobalaVariabler:ttHandleValdaDyn:TEMP-TABLE-PREPARE("VA" + vallistabufftth:TABLE).
      Guru.GlobalaVariabler:ttBuffHandleValdaDyn = Guru.GlobalaVariabler:ttHandleValdaDyn:DEFAULT-BUFFER-HANDLE. 
   END METHOD.
   
   
   METHOD PUBLIC STATIC VOID CreateCustomQuery(INPUT tth AS HANDLE,INPUT q AS CHARACTER):
      IF q = "" THEN q = "FOR EACH " + tth:TABLE.
      CREATE QUERY Guru.GlobalaVariabler:CustomQueryDyn.
      Guru.GlobalaVariabler:CustomQueryDyn:SET-BUFFERS(tth).
      Guru.GlobalaVariabler:CustomQueryDyn:QUERY-PREPARE(q).
      Guru.GlobalaVariabler:CustomQueryDyn:QUERY-OPEN().
      
   END METHOD.
    
   METHOD PUBLIC STATIC VOID HandleKopieraKlistraglobal():
       CREATE TEMP-TABLE Guru.GlobalaVariabler:ttHandleKopieraKlistraglobal.
   END METHOD.   
   METHOD PUBLIC STATIC VOID AmericanSet():
      Guru.GlobalaVariabler:numformat = SESSION:NUMERIC-FORMAT.
      SESSION:NUMERIC-FORMAT = "AMERICAN".
      SetNum().
   END METHOD.   
   METHOD PUBLIC STATIC VOID EuropeanSet():
      Guru.GlobalaVariabler:numformat = SESSION:NUMERIC-FORMAT.
      SESSION:NUMERIC-FORMAT = "EUROPEAN".
      SetNum().
   END METHOD. 
   METHOD PUBLIC STATIC VOID EuropeanAmericanReset():
      IF Guru.GlobalaVariabler:numformat = "" THEN SESSION:NUMERIC-FORMAT = "AMERICAN". 
      ELSE SESSION:NUMERIC-FORMAT = Guru.GlobalaVariabler:numformat. 
      SetNum().     
   END METHOD.
   METHOD PUBLIC STATIC CHARACTER EuropeanAmericanGet():
      RETURN SESSION:NUMERIC-FORMAT.      
   END METHOD.
   METHOD PUBLIC STATIC VOID SetNum():
      DEFINE VARIABLE gc AS CLASS System.Globalization.CultureInfo.
      gc = NEW System.Globalization.CultureInfo("sv-SE",TRUE).
      IF SESSION:NUMERIC-FORMAT = "AMERICAN" THEN gc:NumberFormat:NumberDecimalSeparator = ".".
      ELSE IF SESSION:NUMERIC-FORMAT = "EUROPEAN" THEN gc:NumberFormat:NumberDecimalSeparator = ",".
      System.Threading.Thread:CurrentThread:CurrentCulture = gc.
      
   END METHOD.
   METHOD PUBLIC STATIC VOID StoppaOmrSek(tthandle AS HANDLE):
      IF AppSpringSet[1] = "ONEWWW" OR  AppSpringSet[1] = "VSWWW" THEN DO:
         /*JUST NU KÖRS INTE DETTA PÅ omrtemp kolla i OMRHMT.I */
         
         IF Guru.Konstanter:appcon THEN DO:
            RUN STOPPAOMRSEKLADD.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT  (INPUT Guru.Konstanter:globanv, INPUT-OUTPUT TABLE-HANDLE tthandle).
         END.
         ELSE DO:
            RUN STOPPAOMRSEKLADD.P  (INPUT Guru.Konstanter:globanv, INPUT-OUTPUT TABLE-HANDLE tthandle).
         END.
      END.
         
  END METHOD.
    
 METHOD PUBLIC STATIC VOID Omstart(INPUT huvm AS LOGICAL):
    
    /*status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("WAIT").*/
    
    IF appcon = TRUE THEN DO: 
       IF VALID-HANDLE(apphand) THEN.
       ELSE DO:
          OmstartMess(huvm).
          RETURN. 
       END.
       IF apphand:CONNECTED() THEN DO:
          IF VALID-HANDLE(apphand) THEN .
          ELSE DO:
          OmstartMess(huvm).
          RETURN. 
       END.
       END.
       ELSE DO:
          OmstartMess(huvm).
          RETURN. 
       END.
          
    END.
 END METHOD.
 METHOD PUBLIC STATIC VOID OmstartMess(INPUT huvm AS LOGICAL):
    DEFINE VARIABLE status-mus3 AS LOGICAL NO-UNDO. 
    status-mus3 = SESSION:SET-WAIT-STATE("GENERAL").
    globanvbyt = {LOSENKOLLFEL10.I}.
    IF huvm = TRUE THEN DO:
       MESSAGE "Du har tappat kontakten med databasen. Du loggas in på nytt!"
                VIEW-AS ALERT-BOX.
       
        RUN avb_UI IN MultiStarth.
    END.
    RETURN.  
 END METHOD.
 
 METHOD PUBLIC STATIC VOID ExtradataLaddTracking():
      Guru.SharedVariable:ExtradataDsbufftH = Guru.SharedVariable:ExtraDataDS:GET-BUFFER-HANDLE(1).
      ExtradataTracking(TRUE).
 END METHOD.
 
 METHOD PUBLIC STATIC VOID ExtraDataDSSpar(INPUT kollxml AS LOGICAL):
    DEFINE VARIABLE chDS AS HANDLE NO-UNDO.
    ExtradataTracking(FALSE).
    CREATE DATASET chDS.
    chDS:CREATE-LIKE(Guru.SharedVariable:ExtraDataDS).
    chDS:GET-CHANGES(Guru.SharedVariable:ExtraDataDS).
    IF kollxml = TRUE THEN DO:
       DEFINE VARIABLE SPARAXML AS CHARACTER NO-UNDO.
       MESSAGE "Sparas i "  SESSION:TEMP-DIRECTORY
       VIEW-AS ALERT-BOX.
       SPARAXML = SESSION:TEMP-DIRECTORY + "allaposter.xml". 
       Guru.SharedVariable:ExtraDataDS :WRITE-XML("FILE", SPARAXML).
       SPARAXML = SESSION:TEMP-DIRECTORY + "diffposter.xml".
       
       chDS:WRITE-XML("FILE", SPARAXML).
    END.   
    RUN SparaDynDSstart_UI IN Guru.SharedVariable:ExtradataAppServerHandle (INPUT "ExtraDataDS", INPUT DATASET-HANDLE chDS).
  
    chDS:MERGE-CHANGES(Guru.SharedVariable:ExtraDataDS).
    ExtradataTracking(TRUE).
 END METHOD.

 METHOD PUBLIC STATIC VOID ExtradataTracking(INPUT onoff AS LOGICAL):
    IF NOT VALID-HANDLE(Guru.SharedVariable:ExtradataDsttH) THEN Guru.SharedVariable:ExtradataDsttH = Guru.SharedVariable:ExtradataDsbufftH:TABLE-HANDLE.
    Guru.SharedVariable:ExtradataDsttH:TRACKING-CHANGES = onoff.
   
 END METHOD. 
 METHOD PUBLIC STATIC VOID AvslutaExtradata():
    IF VALID-HANDLE(Guru.SharedVariable:ExtradataAppServerHandle) THEN DO:
       RUN AvslutaExtradata_UI IN Guru.SharedVariable:ExtradataAppServerHandle.
       DELETE PROCEDURE Guru.SharedVariable:ExtradataAppServerHandle NO-ERROR.
    END.
    Guru.SharedVariable:ExtradataAppServerHandle = ?.
 
 END METHOD.
  
 METHOD PUBLIC STATIC VOID setKonst(INPUT apphand AS HANDLE):
      
      MESSAGE "app!"
      VIEW-AS ALERT-BOX.
 END METHOD.
    
  
END CLASS.

 /* 
 
 DEFINE PUBLIC STATIC PROPERTY arendekalk AS CHARACTER NO-UNDO
   PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY KalkArendeText AS CHARACTER  NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
  DEFINE PUBLIC STATIC PROPERTY AppSpringFel AS CHARACTER 
      PUBLIC GET.     PUBLIC SET.
 DEFINE PUBLIC STATIC PROPERTY tillochmeddatum AS DATE
   PUBLIC GET. PUBLIC SET.
 DEFINE PUBLIC STATIC PROPERTY  Guru.GlobalaVariabler:collefth  AS HANDLE 
   PUBLIC GET. PUBLIC SET. 
 DEFINE PUBLIC STATIC PROPERTY  Guru.GlobalaVariabler:colrighth AS HANDLE 
   PUBLIC GET. PUBLIC SET.   
 DEFINE PUBLIC STATIC PROPERTY plusdnr AS INTEGER 
   PUBLIC GET. PUBLIC SET. DEFINE PUBLIC STATIC PROPERTY plusdnr AS INTEGER 
   PUBLIC GET. PUBLIC SET. 
 DEFINE PUBLIC STATIC PROPERTY plusaonr AS CHARACTER 
   PUBLIC GET. PUBLIC SET.  
   DEFINE PUBLIC STATIC PROPERTY plusdnr AS INTEGER 
   PUBLIC GET. PUBLIC SET. 
   DEFINE PUBLIC STATIC PROPERTY plusplannr AS CHARACTER 
   PUBLIC GET. PUBLIC SET.  
   DEFINE PUBLIC STATIC PROPERTY plusartal AS INTEGER 
   PUBLIC GET. PUBLIC SET.  
   DEFINE PUBLIC STATIC PROPERTY plusrec AS RECID  
   PUBLIC GET. PUBLIC SET.  
   DEFINE PUBLIC STATIC PROPERTY plustidrec AS RECID  
   PUBLIC GET. PUBLIC SET.  
   DEFINE PUBLIC STATIC PROPERTY plustid AS DECIMAL 
   PUBLIC GET. PUBLIC SET.  
   DEFINE PUBLIC STATIC PROPERTY plusdval AS LOGICAL 
   PUBLIC GET. PUBLIC SET.  
   DEFINE PUBLIC STATIC PROPERTY succelval AS LOGICAL 
   PUBLIC GET. PUBLIC SET.   
   
 DEFINE PUBLIC STATIC PROPERTY GuruDefaultAnv AS CHARACTER  NO-UNDO
      PUBLIC GET.    PUBLIC SET.
  DEFINE PUBLIC STATIC PROPERTY FranUppf AS LOGICAL  NO-UNDO
      PUBLIC GET.    PUBLIC SET.
 DEFINE PUBLIC STATIC PROPERTY KalkylSomStartiBer AS LOGICAL  NO-UNDO
      PUBLIC GET.    PUBLIC SET.
  DEFINE PUBLIC STATIC PROPERTY DoNotOpenWin AS LOGICAL  NO-UNDO
      PUBLIC GET.    PUBLIC SET.
 DEFINE PUBLIC STATIC PROPERTY globalRunFile     AS CHARACTER   NO-UNDO  
      PUBLIC GET.    PUBLIC SET.
 DEFINE PRIVATE STATIC PROPERTY numformat AS CHARACTER NO-UNDO
   PRIVATE GET.    PRIVATE SET.
 DEFINE PUBLIC STATIC PROPERTY GDPRtyp AS LONGCHAR 
   PUBLIC GET.      PUBLIC SET.
 DEFINE PUBLIC STATIC PROPERTY GDPRvem AS LONGCHAR 
   PUBLIC GET.      PUBLIC SET.   
 
   DEFINE PUBLIC STATIC PROPERTY MaterielTTh AS HANDLE NO-UNDO
      PUBLIC GET.    PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY EMaterielTTh AS HANDLE NO-UNDO
      PUBLIC GET.    PUBLIC SET.   
   DEFINE PUBLIC STATIC PROPERTY ttHandleKopieraKlistraglobal AS HANDLE 
   PUBLIC GET. PUBLIC SET.  
   DEFINE PUBLIC STATIC PROPERTY ttBufferHandleKopieraKlistraglobal AS HANDLE 
   PUBLIC GET. PUBLIC SET.  
  */
   