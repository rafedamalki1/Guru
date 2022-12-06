 
 /*------------------------------------------------------------------------
    File        : GlobalaVariabler
    Purpose     : 
    Syntax      : Guru.GlobalaVariabler:ByggproSid 
    Description : 
    Author(s)   : elpao
    Created     : Tue Apr 24 14:43:00 CEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS Guru.GlobalaVariabler FINAL: 
  /*start*/
   DEFINE PUBLIC STATIC PROPERTY MaterielTTh AS HANDLE NO-UNDO
      PUBLIC GET.    PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY EMaterielTTh AS HANDLE NO-UNDO
      PUBLIC GET.    PUBLIC SET. 
   /*För att klippa och klistar tabbeler method finns i konstanter*/     
   DEFINE PUBLIC STATIC PROPERTY ttHandleKopieraKlistraglobal AS HANDLE 
      PUBLIC GET. PUBLIC SET.  
   DEFINE PUBLIC STATIC PROPERTY retvalkoll AS LOGICAL  
      PUBLIC GET. PUBLIC SET.

   DEFINE PUBLIC STATIC PROPERTY ttBufferHandleKopieraKlistraglobal AS HANDLE 
   PUBLIC GET. PUBLIC SET. 
   
   /*FÖR ATT SKAPA DYNA TABELLER OCH QUERYS ÖVER ALLT */  
   DEFINE PUBLIC STATIC PROPERTY CustomQueryDyn AS HANDLE 
      PUBLIC GET. PUBLIC SET.
    
   /*välj från*/ 
   DEFINE PUBLIC STATIC PROPERTY ttHandleVallistaDyn AS HANDLE 
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY ttBuffHandleVallistaDyn AS HANDLE 
      PUBLIC GET. PUBLIC SET.
   /*valda*/
   DEFINE PUBLIC STATIC PROPERTY ttHandleValdaDyn AS HANDLE 
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY ttBuffHandleValdaDyn AS HANDLE 
      PUBLIC GET. PUBLIC SET.  
   /*ex Guru.Konstanter:CreateVallistaDynTable(INPUT Guru.Konstanter:LeverantorTTh).
   och DynVallista.w
   Guru.GlobalaVariabler:ttBuffHandleValdaDyn:EMPTY-TEMP-TABLE().
   */ 
    /*FÖR ATT SKAPA DYNA TABELLER OCH QUERYS ÖVER ALLT */  
   
   /*för att kunna klocka saker och ting överallt*/
   DEFINE PUBLIC STATIC PROPERTY tidtagninghelp AS INTEGER 
      PUBLIC GET. PUBLIC SET.  
   /*ex
   Guru.GlobalaVariabler:tidtagninghelp = ETIME.
      THIS-OBJECT:StartaControl().
      MESSAGE ETIME - Guru.GlobalaVariabler:tidtagninghelp  "3.5" /*THIS-OBJECT:Name  THIS-PROCEDURE:NAME */
      VIEW-AS ALERT-BOX.
   */
   
   DEFINE PUBLIC STATIC PROPERTY regdatum AS DATE 
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY regdagnamn AS CHARACTER  
      PUBLIC GET. PUBLIC SET. 
   DEFINE PUBLIC STATIC PROPERTY regvnr AS INTEGER   
      PUBLIC GET. PUBLIC SET.
   
   DEFINE PUBLIC STATIC PROPERTY GDPRtyp AS LONGCHAR 
      PUBLIC GET.      PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY GDPRvem AS LONGCHAR 
      PUBLIC GET.      PUBLIC SET.   
   
   DEFINE PUBLIC STATIC PROPERTY numformat AS CHARACTER 
       PUBLIC GET.    PUBLIC SET.
   
   DEFINE PUBLIC STATIC PROPERTY globalRunFile     AS CHARACTER   NO-UNDO  
      PUBLIC GET.    PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY DoNotOpenWin AS LOGICAL  NO-UNDO
      PUBLIC GET.    PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY KalkylSomStartiBer AS LOGICAL  NO-UNDO
      PUBLIC GET.    PUBLIC SET.    
   DEFINE PUBLIC STATIC PROPERTY FranUppf AS LOGICAL  NO-UNDO
      PUBLIC GET.    PUBLIC SET.     
   DEFINE PUBLIC STATIC PROPERTY GuruDefaultAnv AS CHARACTER  NO-UNDO
      PUBLIC GET.    PUBLIC SET. 
   
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
   
   DEFINE PUBLIC STATIC PROPERTY  collefth  AS HANDLE 
      PUBLIC GET. PUBLIC SET. 
   DEFINE PUBLIC STATIC PROPERTY  colrighth AS HANDLE 
      PUBLIC GET. PUBLIC SET. 
      
     
   DEFINE PUBLIC STATIC PROPERTY globsidl AS INTEGER     /* SID BREDD  sätts i SKRIVVAL.W SKRIVVALt.W Sidlangd.w  via SKRIVAPPT.P  o SKRIVAPP.P via input output */ 
      PUBLIC GET. PUBLIC SET.  
   DEFINE PUBLIC STATIC PROPERTY globsids AS INTEGER  /* SID HÖJ  */
      PUBLIC GET. PUBLIC SET.   
   DEFINE PUBLIC STATIC PROPERTY AppSpringFel AS CHARACTER 
      PUBLIC GET. PUBLIC SET.
   
   DEFINE PUBLIC STATIC PROPERTY arendekalk AS CHARACTER NO-UNDO
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY KalkArendeText AS CHARACTER  NO-UNDO
      PUBLIC GET. PUBLIC SET.
     
   DEFINE PUBLIC STATIC PROPERTY LastDialogh AS HANDLE   /*FÖR ATT kunna bugga handels*/  
      PUBLIC GET. PUBLIC SET.    
                              
  
   DEFINE PUBLIC STATIC PROPERTY hjaloforfixh AS HANDLE     
      PUBLIC GET. PUBLIC SET.                             /*FÖR ATT kunna bugga handels*/
  
   DEFINE PUBLIC STATIC PROPERTY StartRadForKnappar AS DECIMAL  
      PUBLIC GET. PUBLIC SET.
   /*Anders Olsson Elpool i Umeå AB  15 aug 2019 09:11:16 
     ska återställas till 0.1 efter med
     Guru.Konstanter:MellanRumKnappar(0.1).
      
   */
   DEFINE PUBLIC STATIC PROPERTY MellanRumKnappar AS DECIMAL  
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY StartKolumnForKnappar AS DECIMAL  
      PUBLIC GET. PUBLIC SET.                             /*samma start på fönster*/
   DEFINE PUBLIC STATIC PROPERTY BtnOkh AS HANDLE     
      PUBLIC GET. PUBLIC SET.                             /*FÖR ATT FLYTTA BTN_OK OCH BTN_AVB BREDVID VARANDRA*/
   DEFINE PUBLIC STATIC PROPERTY BtnAvbh AS HANDLE     
      PUBLIC GET. PUBLIC SET.                             /*FÖR ATT FLYTTA BTN_OK OCH BTN_AVB BREDVID VARANDRA*/
   DEFINE PUBLIC STATIC PROPERTY windowsizeh AS HANDLE     
      PUBLIC GET. PUBLIC SET.  
   DEFINE PUBLIC STATIC PROPERTY fonstercol AS INTEGER 
      PUBLIC GET. PUBLIC SET.                             /*samma start på fönster*/
   DEFINE PUBLIC STATIC PROPERTY fonsterrow AS INTEGER 
      PUBLIC GET. PUBLIC SET.
   
  /*Anders Olsson Elpool i Umeå AB  3 maj 2019 12:29:16 
   fonsteHojd fonsteBredd ska resetats direkt efter användadet
   IF Guru.GlobalaVariabler:fonsteHojd > 0 THEN DO:
      y-mult = Guru.GlobalaVariabler:fonsteHojd / (ph_frame:HEIGHT-PIXELS + 50).
      Guru.GlobalaVariabler:fonsteHojd = 0.
   END.  
  */
  
   DEFINE PUBLIC STATIC PROPERTY fonsteHojd AS INTEGER 
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY fonsteBredd AS INTEGER 
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY FleraBildskarmar AS LOGICAL 
      PUBLIC GET. PUBLIC SET.                             /*samma start på fönster*/
   DEFINE PUBLIC STATIC PROPERTY avbryt_musz AS LOGICAL 
      PUBLIC GET. PUBLIC SET.                             /*samma SOM MUSZ men man slipper ha en delad var*/
   
   DEFINE PUBLIC STATIC PROPERTY Kryptonit AS LOGICAL 
      PUBLIC GET. PUBLIC SET.   
   
   DEFINE PUBLIC STATIC PROPERTY TTRECID AS RECID 
      PUBLIC GET. PUBLIC SET. 
   
   DEFINE PUBLIC STATIC PROPERTY ShowLevelsRow AS INTEGER 
     PUBLIC GET. PUBLIC SET.   
   
   /*beredning*/
   
   DEFINE PUBLIC STATIC PROPERTY LastEnr AS CHARACTER  
      PUBLIC GET. PUBLIC SET.                              /*sista markerade enret i sökta materiel*/
   
   DEFINE PUBLIC STATIC PROPERTY BeredningsNummer AS INTEGER   /*för veta vilken beredning*/ 
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY BeredningsOmrade AS CHARACTER   /*för veta vilken beredning**/ 
      PUBLIC GET. PUBLIC SET.
   
   DEFINE PUBLIC STATIC PROPERTY BeredningsNamn AS CHARACTER   /*för veta vilken beredning**/ 
      PUBLIC GET. PUBLIC SET.
   
   DEFINE PUBLIC STATIC PROPERTY Egengkontroll AS LOGICAL /*För att veta om engenkont ska med */ 
      PUBLIC GET. PUBLIC SET.
   
   DEFINE PUBLIC STATIC PROPERTY EgenskaperTillByggkontroll AS LOGICAL  EXTENT 10  /*För att skicka ner till appserver */ 
      PUBLIC GET. PUBLIC SET.
    
   /*schakt*/
   DEFINE PUBLIC STATIC PROPERTY RubrikSortSchakt AS INTEGER   
      PUBLIC GET. PUBLIC SET.                              /*hur rubriker ska sorteras i schakten*/
   DEFINE PUBLIC STATIC PROPERTY SprofilAuto AS INTEGER   
      PUBLIC GET. PUBLIC SET.                              /*auto av massor mm*/
   
   DEFINE PUBLIC STATIC PROPERTY KalkyUppIschakt AS LOGICAL   
      PUBLIC GET. PUBLIC SET.                              /*INGA HÄNDELSER BARA KALKYLKODER*/
   
   /*BER SCHAKT*/
   DEFINE PUBLIC STATIC PROPERTY NyBerKalkyl AS LOGICAL /*ska en ny berkalkskaps */ 
      PUBLIC GET. PUBLIC SET.
   
   /*BER*/
   DEFINE PUBLIC STATIC PROPERTY KorBerDS AS LOGICAL /*kör med dataset */ 
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY BerDSapph AS HANDLE /*ska en ny berds*/ 
      PUBLIC GET. PUBLIC SET.
   
   DEFINE PUBLIC STATIC PROPERTY BerValDI2h AS HANDLE /*ska en ny berds*/ 
      PUBLIC GET. PUBLIC SET.
   
   DEFINE PUBLIC STATIC PROPERTY copyfriid AS LOGICAL /*VID COPY AV KONSTRUKTIONER */ 
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY copyfrikort AS LOGICAL /*VID COPY AV KONSTRUKTIONER */ 
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY SkapGaTill AS CHARACTER  
      PUBLIC GET. PUBLIC SET. 
   DEFINE PUBLIC STATIC PROPERTY Internetlev AS CHARACTER  /*för att kunna söka från annan lev än vald lev*/ 
      PUBLIC GET. PUBLIC SET. 
   
   DEFINE PUBLIC STATIC PROPERTY KundListId AS INTEGER   /* se UTBBEREDNINGUID.I eller för veta vilket utbyteslista  RUN stpbnettomark_UI*/ 
      PUBLIC GET. PUBLIC SET.
   
   DEFINE PUBLIC STATIC PROPERTY SchaktSid AS INTEGER   /*för veta vilket schakt**/ 
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY ByggproSid AS INTEGER   /*sidor i ett byggprot*/ 
      PUBLIC GET. PUBLIC SET.    
 
   /*mtrl -INKÖP-BERE*/
   DEFINE PUBLIC STATIC PROPERTY LevkodtabInkfalth AS HANDLE NO-UNDO  /*FINNS LEVKODINK*/
      PUBLIC GET.    PUBLIC SET.
  /*enrjmf*/
   DEFINE PUBLIC STATIC PROPERTY jmfPrisEnrTTh AS HANDLE NO-UNDO /*START buffers  FÖR VILKA ENR SOM SKA JÄMFÖRAS.*/
      PUBLIC GET.    PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY jmfPrisEnrBuffTTh AS HANDLE NO-UNDO  /* buffers FÖR VILKA ENR SOM SKA JÄMFÖRAS buffer .*/
      PUBLIC GET.    PUBLIC SET.   
   DEFINE PUBLIC STATIC PROPERTY jmfLevantorBuffh AS HANDLE NO-UNDO  /*vilka leverantöreR ska med i jmf och deras enr .*/   
      PUBLIC GET.    PUBLIC SET.  
      
   /*inköp*/
   DEFINE PUBLIC STATIC PROPERTY GuruDepa AS LOGICAL /*kan ta från flera depåer */ 
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY GuruDepaNr AS INTEGER  /*vald depå */ 
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY GuruDepaNamn AS CHARACTER  /*vald depånamn */ 
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY InkDirekt AS LOGICAL  
      PUBLIC GET. PUBLIC SET.                             /*om man kör utan beredning Guru.Konstanter:beresekvar[16]*/
   DEFINE PUBLIC STATIC PROPERTY DirektAo AS CHARACTER 
      PUBLIC GET. PUBLIC SET.                             /*om man kör utan beredning Guru.Konstanter:beresekvar[16]*/
   DEFINE PUBLIC STATIC PROPERTY DirektDelnr AS INTEGER 
      PUBLIC GET. PUBLIC SET.                             /*om man kör utan beredning Guru.Konstanter:beresekvar[16]*/
  
   /*DEPÅ*/
   DEFINE PUBLIC STATIC PROPERTY GuruProjdepnr AS INTEGER  /*vilket projektlager ska kopplas till */ 
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY GuruVdepnr AS INTEGER  /*depå för markerad beställning /uttag */ 
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY GuruVlplats AS INTEGER  /*vald lagerplats */ 
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY tdSetapph AS HANDLE /*Trummdsapp */ 
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY ClienttdSetapph AS HANDLE /*Trummdsclient */ 
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY LastTimeDepa AS INTEGER    /*för URLOGGNING AV DEPÅ**/ 
      PUBLIC GET. PUBLIC SET. 
      
   /* Datepicker */
   
   DEFINE PUBLIC STATIC PROPERTY pickdatum AS DATE 
      PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC STATIC PROPERTY visapicktid AS LOGICAL 
      PUBLIC GET. PUBLIC SET.      
   DEFINE PUBLIC STATIC PROPERTY picktid AS INTEGER /* sek efter midnatt */
      PUBLIC GET. PUBLIC SET.
   
   
   /*tidskrivning*/
   /*
   DEFINE PUBLIC STATIC PROPERTY gvisatidpermanad AS LOGICAL 
   PUBLIC GET. PUBLIC SET.
   */
   /*personal*/
   /*kalkyl*/     
END CLASS.
