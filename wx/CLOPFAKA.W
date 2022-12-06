&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-3



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-3 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 04/21/97 -  2:05 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE VAR infakplannr AS INTEGER NO-UNDO.
infakplannr = 6.
/* Local Variable Definitions ---                                       */
&Scoped-define NEW NEW SHARED 
DEFINE {&NEW} VARIABLE varforetypval AS INTEGER EXTENT 100 NO-UNDO.     
DEFINE {&NEW} VARIABLE varforetypchar AS CHARACTER EXTENT 100 NO-UNDO.     
DEFINE {&NEW} {&SHARED} TEMP-TABLE kundfodrantemp
   FIELD KUNDKONTOID AS INTEGER  
   FIELD KUNDKONTO   AS CHARACTER
   FIELD KUNDFODTEXT AS CHARACTER
   FIELD BORT        AS LOGICAL
   INDEX KUNDKONTO AS PRIMARY KUNDKONTO
   INDEX KUNDKONTOID KUNDKONTOID BORT.

DEFINE {&NEW} {&SHARED} TEMP-TABLE intakttemp
   FIELD INTAKTID    AS INTEGER   
   FIELD INTAKTKONTO AS CHARACTER 
   FIELD INTAKTTEXT  AS CHARACTER 
   FIELD BORT        AS LOGICAL   
   INDEX INTAKTKONTO AS PRIMARY INTAKTKONTO
   INDEX INTAKTID INTAKTID BORT.
   
DEFINE {&NEW} {&SHARED} TEMP-TABLE motparttemp
   FIELD MOTPARTID   AS INTEGER   
   FIELD MOTPART     AS CHARACTER 
   FIELD MOTPARTTEXT AS CHARACTER 
   FIELD BORT        AS LOGICAL   
   INDEX MOTPART AS PRIMARY MOTPART
   INDEX MOTPARTID MOTPARTID BORT.

DEFINE {&NEW} {&SHARED} TEMP-TABLE momstemp
   FIELD MOMSID      AS INTEGER   
   FIELD MOMSKOD     AS CHARACTER LABEL "Momskonto"
   FIELD MOMSTEXT    AS CHARACTER LABEL "Text"
   FIELD MOMSINTERNT AS DECIMAL 
   FIELD MOMSEXTERNT AS DECIMAL  LABEL "Moms i %"
   FIELD BORT        AS LOGICAL  
   FIELD MOMSNR      AS CHARACTER LABEL "Moms kod"
   INDEX MOMSKOD AS PRIMARY MOMSKOD
   INDEX MOMSID MOMSID BORT.

DEFINE {&NEW} {&SHARED} TEMP-TABLE styrkfmtemp
   FIELD STYRVAL     AS INTEGER
   FIELD KUNDID      AS INTEGER
   FIELD KUNDKONTOID AS INTEGER
   FIELD FAKTTYPID   AS INTEGER
   FIELD INTAKTID    AS INTEGER
   FIELD MOMSID      AS INTEGER
   FIELD MOTPARTID   AS INTEGER
   FIELD KUNDTYP     AS CHARACTER   
   FIELD KUNDFODTEXT AS CHARACTER   
   FIELD KUNDKONTO   AS CHARACTER   
   FIELD FAKTTYPTEXT AS CHARACTER   
   FIELD MOMSKOD     AS CHARACTER   
   FIELD INTAKTKONTO AS CHARACTER   
   FIELD MOTPART     AS CHARACTER   
   FIELD STYRREC     AS RECID       
   INDEX KUNDTYP AS PRIMARY KUNDTYP.

DEFINE {&NEW} {&SHARED} TEMP-TABLE kundtyptemp
   FIELD KUNDID      AS INTEGER   
   FIELD KUNDTYP     AS CHARACTER 
   INDEX KUNDID AS PRIMARY KUNDID.

DEFINE {&NEW} {&SHARED} TEMP-TABLE faktureringstyptemp
   FIELD FAKTTYPID      AS INTEGER
   FIELD FAKTTYPTEXT    AS CHARACTER   
   FIELD OBLMOMS        AS LOGICAL   
   FIELD SLUT           AS LOGICAL
   FIELD TIDIGAREACONTO AS LOGICAL
   FIELD TIDIGAREFAKT   AS LOGICAL
   FIELD TIDIGAREMOMS   AS LOGICAL
   INDEX FAKTTYPID AS PRIMARY FAKTTYPID.

DEFINE TEMP-TABLE faktmomstemp NO-UNDO   
   FIELD AONR  AS CHARACTER
   FIELD BELOPP AS DECIMAL
   FIELD DELNR AS INTEGER
   FIELD FAKTNR AS INTEGER
   FIELD FDELNR AS INTEGER
   FIELD FRITEXT AS CHARACTER
   FIELD MOMSBELOPP AS DECIMAL
   FIELD MOMSEXTERNT AS DECIMAL
   FIELD MOMSID      AS INTEGER
   FIELD TYP      AS CHARACTER
   FIELD VFAKTNR  AS INTEGER
   FIELD MOMSKOD        AS CHARACTER 
   FIELD MOMSTEXT       AS CHARACTER
   FIELD FROW              AS ROWID
   INDEX FAKTNR IS PRIMARY FAKTNR FDELNR.
DEFINE TEMP-TABLE faktinkontotemp NO-UNDO 
   FIELD AONR        AS CHARACTER
   FIELD BELOPP      AS DECIMAL
   FIELD DELNR       AS INTEGER
   FIELD FAKTNR      AS INTEGER
   FIELD FDELNR      AS INTEGER
   FIELD INTAKTID    AS INTEGER
   FIELD MOTPARTID   AS INTEGER
   FIELD VFAKTNR     AS INTEGER
   FIELD INTAKTKONTO       AS CHARACTER
   FIELD INTAKTTEXT        AS CHARACTER
   FIELD MOTPART           AS CHARACTER
   FIELD MOTPARTTEXT       AS CHARACTER
   FIELD FROW              AS ROWID
   INDEX FAKTNR IS PRIMARY FAKTNR FDELNR.
DEFINE TEMP-TABLE faktkundtemp NO-UNDO 
   FIELD AONR                AS CHARACTER
   FIELD BELOPP              AS DECIMAL  
   FIELD DELNR               AS INTEGER  
   FIELD FAKTNR              AS INTEGER  
   FIELD FAKTDATUM           AS DATE
   FIELD FDATUM           AS DATE
   FIELD FDELNR              AS INTEGER  
   FIELD KUNDKONTOID         AS INTEGER  
   FIELD MOTPARTID           AS INTEGER  
   FIELD VFAKTNR            AS INTEGER  
   FIELD KUNDKONTO   AS CHARACTER
   FIELD KUNDFODTEXT AS CHARACTER
   FIELD MOTPART     AS CHARACTER
   FIELD MOTPARTTEXT AS CHARACTER
   FIELD FROW              AS ROWID
   INDEX FAKTNR IS PRIMARY FAKTNR FDELNR.
DEFINE TEMP-TABLE efaktkundtemp NO-UNDO LIKE faktkundtemp.
DEFINE TEMP-TABLE efaktinkontotemp NO-UNDO LIKE faktinkontotemp.
DEFINE TEMP-TABLE efaktmomstemp NO-UNDO LIKE faktmomstemp.

&Scoped-define NEW NEW 
DEFINE {&NEW} SHARED TEMP-TABLE gfaktemp NO-UNDO
  FIELD TYPTEXT AS CHARACTER FORMAT "X(8)" LABEL "Uppgifter"  
  FIELD TOTALT AS DECIMAL FORMAT "->>>>>>>9" LABEL "Totalt"  
  FIELD MTRL AS DECIMAL FORMAT "->>>>>>>9" LABEL "Materiel kostnad"
  FIELD OVRIG AS DECIMAL FORMAT "->>>>>>>9" LABEL "Övrig kostnad"
  FIELD ARBKOST AS DECIMAL FORMAT "->>>>>>>9" LABEL "Arbets kostnad"
  FIELD OBELOPP AS DECIMAL FORMAT "->>>>>>>9" LABEL "Övertid kostnad"
  FIELD TRAKT AS DECIMAL FORMAT "->>>>>>>9" LABEL "Traktamentes kostnad"
  FIELD LONTILL AS DECIMAL FORMAT "->>>>>>>9" LABEL "Lönetilläggs kostnad"
  FIELD RES AS DECIMAL FORMAT "->>>>>>>9" LABEL "Res kostnad"
  FIELD KBELOPP AS DECIMAL FORMAT "->>>>>>>9" LABEL "Fr.tjänst kost."
  FIELD ORDNING AS DECIMAL FORMAT "->,>>>,>>9" 
  INDEX ORDNING IS PRIMARY ORDNING. 

DEFINE {&NEW} SHARED VARIABLE gavdl AS CHARACTER NO-UNDO. /*AVDELNING LÅNG*/
DEFINE {&NEW} SHARED VARIABLE gavdk AS CHARACTER NO-UNDO.     /*AVDELNING KORT*/
DEFINE {&NEW} SHARED VARIABLE gomrl AS CHARACTER NO-UNDO.  /*OMRÅDE LÅNG*/
DEFINE {&NEW} SHARED VARIABLE gomrk AS CHARACTER NO-UNDO.      /*OMRÅDE KORT*/
DEFINE {&NEW} SHARED VARIABLEgaol AS CHARACTER NO-UNDO.   /*AONR LÅNG*/
DEFINE {&NEW} SHARED VARIABLEgaok AS CHARACTER NO-UNDO.       /*AONR KORT*/   
DEFINE {&NEW} SHARED VARIABLEgpll AS CHARACTER NO-UNDO.  /*PLANNR LÅNG*/
DEFINE {&NEW} SHARED VARIABLEgplk AS CHARACTER NO-UNDO.     /*PLANNR KORT*/
DEFINE {&NEW} SHARED VARIABLEgenl AS CHARACTER NO-UNDO. /*E-NUMMER LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLEgenk AS CHARACTER NO-UNDO.  /*E-NUMMER KORT*/
DEFINE {&NEW} SHARED VARIABLE gjul AS CHARACTER NO-UNDO. /*JURIDISK LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLEgjuk AS CHARACTER NO-UNDO.  /*JURIDISK KORT*/
DEFINE {&NEW} SHARED VARIABLEgfastl AS CHARACTER NO-UNDO. /*FASTA LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLE gfastk AS CHARACTER NO-UNDO.     /*FASTA KORT*/
DEFINE {&NEW} SHARED VARIABLE gtilll AS CHARACTER NO-UNDO. /*TILLFÄLIGA LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLE gtillk AS CHARACTER NO-UNDO.     /*TILLFÄLIGA KORT*/
DEFINE {&NEW} SHARED VARIABLEgberel AS CHARACTER NO-UNDO. /*BEREDARE LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLEgberek AS CHARACTER NO-UNDO.     /*BEREDARE KORT*/
DEFINE {&NEW} SHARED VARIABLE gprojl AS CHARACTER NO-UNDO. /*PROJEKTÖR LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLE gprojk AS CHARACTER NO-UNDO.     /*PROJEKTÖR KORT*/
DEFINE {&NEW} SHARED VARIABLE garbal AS CHARACTER NO-UNDO. /*ARBETSANSVARIG LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLE garbak AS CHARACTER NO-UNDO.     /*ARBETSANSVARIG KORT*/
DEFINE {&NEW} SHARED VARIABLE gutfk AS CHARACTER NO-UNDO.     /*Utförande KORT*/
DEFINE {&NEW} SHARED VARIABLE gutfl AS CHARACTER NO-UNDO. /*Utförande LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLE gbestk AS CHARACTER NO-UNDO.     /*Beställare/Kund KORT*/
DEFINE {&NEW} SHARED VARIABLE gbestl AS CHARACTER NO-UNDO. /*Beställare/Kund LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLE gdebk AS CHARACTER NO-UNDO.     /*Debiterigstyp KORT*/
DEFINE {&NEW} SHARED VARIABLE gdebl AS CHARACTER NO-UNDO.  /*Debiterigstyp LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLE gtidlk AS CHARACTER NO-UNDO.     /*tidläge KORT*/
DEFINE {&NEW} SHARED VARIABLE gtidll AS CHARACTER NO-UNDO.  /*tidläge LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLE gutfardk AS CHARACTER NO-UNDO.     /*utfärdat av KORT*/
DEFINE {&NEW} SHARED VARIABLE gutfardl AS CHARACTER NO-UNDO.  /*utfärdat av LÅNG*/ 
DEFINE {&NEW} SHARED VARIABLE grefbefk AS CHARACTER NO-UNDO.     /*Ref.nr beställare KORT*/
DEFINE {&NEW} SHARED VARIABLE grefbefl AS CHARACTER NO-UNDO.  /*Ref.nr beställare  LÅNG*/ 


DEFINE VARIABLE brwproc AS HANDLE EXTENT 25 NO-UNDO.
   /*GLOBVAR2.I*/
   DEFINE {&NEW} SHARED VARIABLE globanv AS CHARACTER NO-UNDO. 
   DEFINE {&NEW} SHARED VARIABLE globanvavdnr AS INTEGER NO-UNDO. 
   DEFINE {&NEW} SHARED VARIABLE globanvpkod AS CHARACTER NO-UNDO. 
   DEFINE {&NEW} SHARED VARIABLE globallm AS LOGICAL NO-UNDO. 
   DEFINE {&NEW} SHARED VARIABLE globniv AS INTEGER NO-UNDO. 
   DEFINE {&NEW} SHARED VARIABLE globallpers AS LOGICAL NO-UNDO. 
   DEFINE {&NEW} SHARED VARIABLE globallao AS LOGICAL NO-UNDO.
   DEFINE {&NEW} SHARED VARIABLE globavd AS INTEGER NO-UNDO. 
   DEFINE {&NEW} SHARED VARIABLE globomr AS CHARACTER NO-UNDO. 
   DEFINE {&NEW} SHARED VARIABLE globlos AS CHARACTER NO-UNDO.
   DEFINE {&NEW} SHARED VARIABLE globpersnamn AS CHARACTER NO-UNDO.
   DEFINE {&NEW} SHARED VARIABLE globforetag AS CHARACTER NO-UNDO.
   DEFINE {&NEW} SHARED VARIABLE gvisatidpermanad AS LOGICAL NO-UNDO.
   DEFINE {&NEW} SHARED VARIABLE globsidl AS INTEGER NO-UNDO.
   DEFINE {&NEW} SHARED VARIABLE globsids AS INTEGER NO-UNDO.
   DEFINE {&NEW} SHARED VARIABLE Guru.GlobalaVariabler:plusaonr AS CHARACTER NO-UNDO.
   DEFINE {&NEW} SHARED VARIABLE Guru.GlobalaVariabler:plusdnr AS INTEGER NO-UNDO.
   DEFINE {&NEW} SHARED VARIABLE plusrec AS RECID  NO-UNDO.
   DEFINE {&NEW} SHARED VARIABLE plustidrec AS RECID  NO-UNDO.
   DEFINE {&NEW} SHARED VARIABLE plustid AS DECIMAL NO-UNDO.
   DEFINE {&NEW} SHARED VARIABLE plusdval AS LOGICAL NO-UNDO.
   DEFINE {&NEW} SHARED VARIABLE succelval AS LOGICAL NO-UNDO. 
   


   DEFINE VARIABLE tillochmeddatum AS DATE NO-UNDO.
   DEFINE TEMP-TABLE felinut
      FIELD ANVANDARE AS CHARACTER
      FIELD DATUM    AS DATE
      FIELD EMOTAGET AS LOGICAL
      FIELD FELKOD   AS INTEGER
      FIELD FELTEXT  AS CHARACTER
      FIELD PROGRAM  AS CHARACTER
      INDEX FELTEXT IS PRIMARY ANVANDARE FELKOD.
   DEFINE {&NEW} SHARED TEMP-TABLE felmeddtemp 
     FIELD FELMEDD AS CHARACTER
     FIELD VAL AS INTEGER.
   /*
     INDEX VAL IS PRIMARY VAL.
     */
   DEFINE {&NEW} SHARED TEMP-TABLE xsektemp
     FIELD AV-LEVEL AS INTEGER
     FIELD MENYVART AS CHARACTER
     FIELD SEK AS LOGICAL EXTENT 20
     INDEX XSEK IS PRIMARY MENYVART AV-LEVEL.



DEFINE NEW SHARED VARIABLE appcon AS LOGICAL NO-UNDO.



 &Scoped-define NEW NEW 
DEFINE {&NEW} SHARED TEMP-TABLE faktyptemp NO-UNDO
   FIELD FAKTTYP   AS CHARACTER 
   FIELD VIFAKTTYP AS CHARACTER
   FIELD TYP       AS INTEGER
   FIELD ORDNING   AS INTEGER 
   INDEX ORDNING IS PRIMARY ORDNING
   INDEX FAKTYP FAKTTYP
   INDEX VIFAKTYP VIFAKTTYP.
FUNCTION faktyp RETURNS CHARACTER
  ( INPUT varfaktyp AS CHARACTER ):
  FIND FIRST faktyptemp WHERE faktyptemp.FAKTTYP = varfaktyp NO-ERROR.
  IF NOT AVAILABLE faktyptemp THEN RETURN "Saknas".
  ELSE RETURN faktyptemp.VIFAKTTYP.

END FUNCTION.

FUNCTION faktypVI RETURNS CHARACTER
  ( INPUT varfaktyp AS CHARACTER ):
  FIND FIRST faktyptemp WHERE faktyptemp.VIFAKTTYP = varfaktyp NO-ERROR.
  IF NOT AVAILABLE faktyptemp THEN RETURN "Saknas".
  ELSE RETURN faktyptemp.FAKTTYP.

END FUNCTION.
DEFINE {&NEW} SHARED TEMP-TABLE faktplantemp  NO-UNDO
   FIELD ALLTTAK                   AS LOGICAL     
   FIELD ANVANDARE                 AS CHARACTER   
   FIELD BESTID                    AS CHARACTER   
   FIELD EGNAOVR                   AS LOGICAL     
   FIELD FAKTNR                    AS INTEGER     
   FIELD FAKTTYP                   AS CHARACTER   
   FIELD FAKTTYPUNDER              AS INTEGER     
   FIELD FDELNR                    AS INTEGER     
   FIELD HANVANDARE                AS CHARACTER   
   FIELD KALKPRIS                  AS DECIMAL     
   FIELD NAMN                      AS CHARACTER   
   FIELD OFFERTPRIS                AS DECIMAL     
   FIELD OMRADE                    AS CHARACTER   
   FIELD PANVANDARE                AS CHARACTER   
   FIELD PRISNUM                   AS INTEGER     
   FIELD PRISNUMO                  AS INTEGER     
   FIELD PROJEKTKOD                AS CHARACTER   
   FIELD RESTORE                   AS DECIMAL     
   FIELD SENASTFAK                 AS DATE        
   FIELD SENASTTID                 AS DATE        
   FIELD SLUTFAKT                  AS LOGICAL     
   FIELD TAKPRIS                   AS DECIMAL       
   FIELD TOTPRIS                   AS DECIMAL       
   FIELD VFAKTNR                   AS INTEGER       
   FIELD VISAONR                   AS CHARACTER 
   FIELD VIBESTID                   AS CHARACTER 
   INDEX FAKTNR IS PRIMARY FAKTNR
   INDEX VIBESTID VIBESTID
   INDEX NAMN NAMN.
DEFINE {&NEW} SHARED TEMP-TABLE vfaktplantemp NO-UNDO LIKE faktplantemp .
DEFINE TEMP-TABLE evfaktplantemp NO-UNDO LIKE faktplantemp .
DEFINE TEMP-TABLE extravfaktplantemp NO-UNDO LIKE faktplantemp 
    FIELD AONR AS CHARACTER
    FIELD DELNR AS INTEGER.
DEFINE TEMP-TABLE faktureradtemp NO-UNDO 
   FIELD FAKTNR AS INTEGER   
   FIELD FDELNR AS INTEGER  
   FIELD NAMN AS CHARACTER 
   FIELD DATUM AS DATE 
   FIELD TOTPRIS AS DECIMAL 
   FIELD TIMMAR AS DECIMAL 
   FIELD BELOPP AS DECIMAL 
   FIELD OBELOPP AS DECIMAL 
   FIELD TBELOPP AS DECIMAL 
   FIELD OTIMMAR AS DECIMAL 
   FIELD LONKOST AS DECIMAL 
   FIELD MTRLKOST AS DECIMAL 
   FIELD OVRKOST AS DECIMAL 
   FIELD KOSTBELOPP AS DECIMAL 
   FIELD FAKTXT AS CHARACTER 
   FIELD ANTALRADER AS integer 
   FIELD RESKOST AS INTEGER 
   FIELD SENASTTID AS DATE 
   FIELD MOMSBELOPP AS DECIMAL 
   FIELD VFAKTNR AS INTEGER 
   FIELD AVGAR AS DECIMAL LABEL "Avgår tidigare fakturerat"
   FIELD ORESUTJ AS DECIMAL 
   FIELD TIDIGARMOMS  AS DECIMAL  
   FIELD RESKOSTDEC  AS DECIMAL  
   FIELD FAKTTYPID AS INTEGER  
   FIELD PRELGOD AS LOGICAL  
   FIELD BILAGOR AS CHARACTER 
   FIELD BOKDATUM  AS DATE FORMAT "99/99/99" INITIAL ?
   INDEX FAKTNR IS PRIMARY FAKTNR FDELNR.
DEFINE TEMP-TABLE faktkredtemp NO-UNDO LIKE faktureradtemp 
   FIELD SPARAD AS LOGICAL
   FIELD VKREDIT AS INTEGER
   FIELD DEBKRED AS LOGICAL
   INDEX VFAKTNR IS PRIMARY FAKTNR VKREDIT VFAKTNR FDELNR DATUM
   INDEX FDELNR FDELNR.
DEFINE BUFFER faktkredtempbuff FOR faktkredtemp.
DEFINE TEMP-TABLE faktdebkred NO-UNDO LIKE faktkredtemp 
   FIELD KDATUM AS DATE 
   FIELD KTOTPRIS AS DECIMAL.
DEFINE TEMP-TABLE efaktkredtemp NO-UNDO LIKE faktkredtemp .
DEFINE TEMP-TABLE faktnamntemp NO-UNDO
   FIELD VKREDIT  AS INTEGER
   FIELD VFAKTNR AS INTEGER                     
   FIELD VARREF  AS CHARACTER                   
   FIELD TEL     AS CHARACTER                   
   FIELD LOPFAST AS LOGICAL                     
   FIELD KONTAKT AS CHARACTER                   
   FIELD FDELNR      AS INTEGER                 
   FIELD FAKTURNR  AS INTEGER                   
   FIELD FAKTURADATUM AS DATE                   
   FIELD FAKPNR  AS CHARACTER                   
   FIELD FAKORT  AS CHARACTER                   
   FIELD FAKADRESS AS CHARACTER                 
   FIELD BESTNAMN  AS CHARACTER                 
   FIELD BESTID    AS CHARACTER                 
   FIELD BESTALLARE AS CHARACTER                
   FIELD ARBOMF     AS CHARACTER                
   INDEX FAKTURNR IS PRIMARY FAKTURNR FDELNR VFAKTNR.
DEFINE TEMP-TABLE efaktnamntemp NO-UNDO LIKE faktnamntemp .
DEFINE TEMP-TABLE faktfriatemp NO-UNDO                                
   FIELD ANTAL        AS DECIMAL  LABEL "Antal"
   FIELD AONR         AS CHARACTER
   FIELD DELNR        AS INTEGER
   FIELD ENHET        AS CHARACTER LABEL "Sort"
   FIELD FAKTNR       AS INTEGER
   FIELD FAKTTEXT     AS CHARACTER LABEL "Fakt.text"
   FIELD FAKTURERAD   AS LOGICAL   LABEL "Ta med" FORMAT "Ja/Nej"
   FIELD FDELNR       AS INTEGER
   FIELD LOPNR        AS INTEGER
   FIELD MOMSEXTERNT  AS DECIMAL
   FIELD MOMSID       AS INTEGER
   FIELD OANTAL       AS DECIMAL
   FIELD OPRIS        AS DECIMAL
   FIELD PRIS_ENHET   AS DECIMAL  LABEL "Pris/enhet"
   FIELD TOTALT       AS DECIMAL  LABEL "Offert"
   FIELD TOTKALK      AS DECIMAL  LABEL "Tot.kalk"
   FIELD TYP          AS CHARACTER
   FIELD VFAKTNR      AS INTEGER
   FIELD VKREDIT  AS INTEGER
   FIELD ROWFRIA  AS ROWID
   INDEX FAKTNR IS PRIMARY FAKTNR FDELNR
   INDEX KREDIT FAKTNR VKREDIT
   INDEX LOPNR LOPNR
   INDEX DEBET FAKTNR VFAKTNR.
DEFINE TEMP-TABLE efaktfriatemp NO-UNDO LIKE faktfriatemp .

DEFINE TEMP-TABLE faktintakkontotemp NO-UNDO 
   FIELD AONR           AS CHARACTER
   FIELD BELOPP         AS DECIMAL
   FIELD DELNR         AS INTEGER
   FIELD FAKTNR        AS INTEGER
   FIELD FDELNR        AS INTEGER
   FIELD INTAKTID      AS INTEGER
   FIELD MOMSID        AS INTEGER
   FIELD MOTPARTID     AS INTEGER
   FIELD OMRADE        AS CHARACTER
   FIELD VFAKTNR       AS INTEGER
   FIELD VKREDIT       AS INTEGER
   INDEX FAKTNR IS PRIMARY FAKTNR FDELNR VFAKTNR
   INDEX VFAKTNR FAKTNR VFAKTNR.

DEFINE TEMP-TABLE kosttemp NO-UNDO                    
   FIELD FAKTNR AS CHARACTER 
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD RADNR AS INTEGER
   FIELD BENAMNING AS CHARACTER
   FIELD BOKKONTO AS CHARACTER
   FIELD PERSKOST AS DECIMAL
   FIELD TRAKTKOST AS DECIMAL
   FIELD MASKKOST AS DECIMAL
   FIELD FRTJPAKR AS DECIMAL
   FIELD MTRL AS DECIMAL
   FIELD MTRLPAKR AS DECIMAL
   FIELD OVRKR AS DECIMAL
   FIELD MED AS LOGICAL FORMAT "Ja/Nej"
   FIELD MOMSEXTERNT AS DECIMAL
   FIELD MOMSID AS INTEGER
   FIELD MOMSKOD AS CHARACTER    
   INDEX AONR IS PRIMARY AONR DELNR FAKTNR.

DEFINE TEMP-TABLE kundregeltemp NO-UNDO
   FIELD BESTID      AS CHARACTER 
   FIELD ENTRAK      AS DECIMAL
   FIELD FAKINT      AS INTEGER
   FIELD FDAGAR      AS INTEGER
   FIELD FLERTRAK    AS DECIMAL
   FIELD FRTJPA      AS DECIMAL
   FIELD INTAKTID    AS INTEGER
   FIELD KOSTREGRGL  AS CHARACTER
   FIELD KUNDID      AS INTEGER
   FIELD KUNDKONTOID  AS INTEGER
   FIELD LONRGL       AS CHARACTER
   FIELD MIL          AS DECIMAL
   FIELD MOMSID       AS INTEGER
   FIELD MOTPARTID    AS INTEGER
   FIELD MTRLPA       AS DECIMAL
   FIELD MTRLRGL      AS CHARACTER
   FIELD OVERTIDRGL   AS CHARACTER
   FIELD OVRKRGL      AS CHARACTER
   FIELD SLUT%        AS INTEGER
   FIELD START%       AS INTEGER
   FIELD TIMRGL       AS CHARACTER
   FIELD TRAKTRGL     AS CHARACTER
   FIELD FAKTNR     AS INTEGER
   FIELD SUMALLAR     AS LOGICAL
   INDEX BESTID IS PRIMARY BESTID
   INDEX FAKTNR FAKTNR.

&Scoped-define NEW NEW                          
DEFINE {&NEW} SHARED VARIABLE FILL-IN-TOMDAT AS DATE FORMAT "99/99/99" NO-UNDO.
DEFINE NEW SHARED VARIABLE vartyp AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE faktupphmth AS HANDLE NO-UNDO.
DEFINE VARIABLE fakkoproch AS HANDLE NO-UNDO. 
DEFINE NEW SHARED VARIABLE fakthmth AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE skarpfaktok AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.   
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE fbestapph AS HANDLE NO-UNDO.
DEFINE VARIABLE sortvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE feltextvar AS CHARACTER FORMAT "X(10)" NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
DEFINE VARIABLE sumanstf AS CHARACTER NO-UNDO.
DEFINE VARIABLE sumanstraf AS CHARACTER NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO. 
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.  
DEFINE VARIABLE arkivvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE gamanv AS CHARACTER NO-UNDO.
DEFINE VARIABLE delfakt AS DECIMAL NO-UNDO.
DEFINE VARIABLE faoff AS DECIMAL NO-UNDO.
DEFINE VARIABLE resber AS DECIMAL NO-UNDO.
DEFINE VARIABLE tidigfakt AS DECIMAL NO-UNDO.
DEFINE VARIABLE frirow AS ROWID NO-UNDO.
DEFINE VARIABLE brwrow AS ROWID NO-UNDO.
DEFINE VARIABLE nya AS LOGICAL NO-UNDO.
DEFINE VARIABLE hamttid AS LOGICAL NO-UNDO.
DEFINE VARIABLE hamtkost AS LOGICAL NO-UNDO.  
DEFINE VARIABLE faktskap AS LOGICAL NO-UNDO.         
DEFINE VARIABLE aonrslut AS LOGICAL NO-UNDO.  
DEFINE VARIABLE aonrstart AS LOGICAL NO-UNDO.   
DEFINE VARIABLE valaconttak AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE rundavar AS LOGICAL NO-UNDO. 
DEFINE VARIABLE fakdatum AS DATE NO-UNDO. 
DEFINE VARIABLE fakslut AS LOGICAL NO-UNDO. 
DEFINE VARIABLE kollvecko AS CHARACTER NO-UNDO.
DEFINE VARIABLE sumpkod AS CHARACTER NO-UNDO.
DEFINE VARIABLE pkod AS CHARACTER NO-UNDO.
DEFINE VARIABLE vareqdag AS INTEGER NO-UNDO.
DEFINE VARIABLE slutvar AS INTEGER NO-UNDO.
DEFINE VARIABLE tidfaktvar AS DECIMAL NO-UNDO.
DEFINE VARIABLE tidmomsvar AS DECIMAL NO-UNDO.
DEFINE VARIABLE nymomstotal AS DECIMAL NO-UNDO.
DEFINE VARIABLE nyfakttotal AS DECIMAL NO-UNDO.
DEFINE VARIABLE skarpvar AS INTEGER NO-UNDO.
DEFINE VARIABLE vararbkost AS DECIMAL NO-UNDO.   
DEFINE VARIABLE vartrakost AS DECIMAL NO-UNDO.
DEFINE VARIABLE sluttotpris AS DECIMAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-C
&Scoped-define BROWSE-NAME BRW_KOST

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES kosttemp

/* Definitions for BROWSE BRW_KOST                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_KOST kosttemp.FAKTNR kosttemp.BENAMNING ~
kosttemp.AONR kosttemp.DELNR kosttemp.BOKKONTO kosttemp.TRAKTKOST ~
kosttemp.PERSKOST kosttemp.MASKKOST kosttemp.FRTJPAKR kosttemp.MTRL ~
kosttemp.MTRLPAKR kosttemp.OVRKR kosttemp.MED 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KOST kosttemp.FAKTNR ~
kosttemp.BENAMNING kosttemp.AONR kosttemp.DELNR kosttemp.BOKKONTO ~
kosttemp.TRAKTKOST kosttemp.PERSKOST kosttemp.MASKKOST kosttemp.FRTJPAKR ~
kosttemp.MTRL kosttemp.MTRLPAKR kosttemp.OVRKR kosttemp.MED 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_KOST kosttemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_KOST kosttemp
&Scoped-define QUERY-STRING-BRW_KOST FOR EACH kosttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KOST OPEN QUERY BRW_KOST FOR EACH kosttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KOST kosttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KOST kosttemp


/* Definitions for FRAME FRAME-C                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN_SLUTFAKT TOG_DIFF BTN_AVB 
&Scoped-Define DISPLAYED-FIELDS vfaktplantemp.FAKTNR vfaktplantemp.FAKTNR ~
vfaktplantemp.NAMN vfaktplantemp.NAMN 
&Scoped-define DISPLAYED-TABLES vfaktplantemp
&Scoped-define FIRST-DISPLAYED-TABLE vfaktplantemp
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_FDELNR-2 FILL-IN_FDELNR ~
FILL-IN_SLUTFAKT FILL-IN_INKOMST TOG_DIFF 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD klockan100 WINDOW-3 
FUNCTION klockan100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD klockan60 WINDOW-3 
FUNCTION klockan60 RETURNS DECIMAL
  ( INPUT ber100 AS DECIMAL ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD runda WINDOW-3 
FUNCTION runda RETURNS DECIMAL
  ( INPUT varedin AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-3 AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-BRW_KOST 
       MENU-ITEM m_Ta_med_markerade_posterKOST LABEL "Ta med markerade poster"
       MENU-ITEM m_Ta_ej_med_markerade_posterKOST LABEL "Ta ej med markerade poster".


/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Spara/Avsluta" 
     SIZE 22.5 BY 1.

DEFINE VARIABLE FILL-IN_FDELNR AS INTEGER FORMAT "999999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_FDELNR-2 AS INTEGER FORMAT "999999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_INKOMST AS DECIMAL FORMAT "->>>>>>>9" INITIAL 0 
     LABEL "intäkter" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE FILL-IN_SLUTFAKT AS LOGICAL FORMAT "Ja/Nej" INITIAL NO 
     LABEL "Slutfakt" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_TAKPRIS AS DECIMAL FORMAT "->>>>>>>>9.99" INITIAL 0 
     LABEL "Takpris" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE RAD_VISA AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tidskrivning", 1,
"Kostnadsreg.", 2,
"Fri komplettering", 3
     SIZE 96.88 BY .83 NO-UNDO.

DEFINE VARIABLE TOG_ACONT AS LOGICAL INITIAL no 
     LABEL "A-contofakt." 
     VIEW-AS TOGGLE-BOX
     SIZE 16.5 BY .63 NO-UNDO.

DEFINE VARIABLE TOG_DIFF AS LOGICAL INITIAL no 
     LABEL "Differentierad moms" 
     VIEW-AS TOGGLE-BOX
     SIZE 23.75 BY .67 NO-UNDO.

DEFINE VARIABLE TOG_MOMS AS LOGICAL INITIAL no 
     LABEL "Kontoberäkningar utförda." 
     VIEW-AS TOGGLE-BOX
     SIZE 29.75 BY .96 TOOLTIP "Om kontoberäkningar är utförda kan inga ändringar göras!" NO-UNDO.

DEFINE VARIABLE TOG_MOMS-2 AS LOGICAL INITIAL no 
     LABEL "Kontoberäkningar utförda." 
     VIEW-AS TOGGLE-BOX
     SIZE 29.75 BY .96 TOOLTIP "Om kontoberäkningar är utförda kan inga ändringar göras!" NO-UNDO.

DEFINE VARIABLE TOG_TAK AS LOGICAL INITIAL no 
     LABEL "Takpris" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.13 BY .63 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_KOST FOR 
      kosttemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_KOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KOST WINDOW-3 _STRUCTURED
  QUERY BRW_KOST NO-LOCK DISPLAY
      kosttemp.FAKTNR COLUMN-LABEL "Ver-nr" FORMAT "x(15)":U WIDTH 13
      kosttemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(22)":U
            WIDTH 17
      kosttemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      kosttemp.DELNR COLUMN-LABEL "Del!nr" FORMAT ">99":U
      kosttemp.BOKKONTO COLUMN-LABEL "B.för.!konto" FORMAT "X(8)":U
      kosttemp.TRAKTKOST COLUMN-LABEL "Trakt.!kost." FORMAT "->>>>>>>9":U
      kosttemp.PERSKOST COLUMN-LABEL "Pers.!kost" FORMAT "->>>>>>>9":U
      kosttemp.MASKKOST COLUMN-LABEL "Fr.tjänst!kost." FORMAT "->>>>>>>9":U
      kosttemp.FRTJPAKR COLUMN-LABEL "Påslag!fr.tjänst" FORMAT "->>>>>>>9":U
      kosttemp.MTRL COLUMN-LABEL "Mtrl.!kr" FORMAT "->>>>>>9":U
      kosttemp.MTRLPAKR COLUMN-LABEL "Påslag!materiel" FORMAT "->>>>>>>9":U
      kosttemp.OVRKR COLUMN-LABEL "Övrig!kostnad" FORMAT "->>>>>>>9":U
      kosttemp.MED COLUMN-LABEL "Ta!med" FORMAT "Ja/Nej":U
  ENABLE
      kosttemp.FAKTNR
      kosttemp.BENAMNING
      kosttemp.AONR
      kosttemp.DELNR
      kosttemp.BOKKONTO
      kosttemp.TRAKTKOST
      kosttemp.PERSKOST
      kosttemp.MASKKOST
      kosttemp.FRTJPAKR
      kosttemp.MTRL
      kosttemp.MTRLPAKR
      kosttemp.OVRKR
      kosttemp.MED
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 124 BY 15
         TITLE "Kostnadsregistrering - ~"Klicka~" på de som ej ska faktureras i MED-fältet".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-C
     vfaktplantemp.FAKTNR AT ROW 1.5 COL 16.38 COLON-ALIGNED
          LABEL "Preliminär nr."
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     vfaktplantemp.FAKTNR AT ROW 1.5 COL 16.38 COLON-ALIGNED
          LABEL "Preliminär nr."
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     FILL-IN_FDELNR-2 AT ROW 1.5 COL 25.5 COLON-ALIGNED NO-LABEL
     FILL-IN_FDELNR AT ROW 1.5 COL 25.5 COLON-ALIGNED NO-LABEL
     vfaktplantemp.NAMN AT ROW 1.5 COL 35.25 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 31 BY 1
     vfaktplantemp.NAMN AT ROW 1.5 COL 35.25 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 31 BY 1
     FILL-IN_SLUTFAKT AT ROW 1.5 COL 87.13 COLON-ALIGNED
     TOG_TAK AT ROW 7.46 COL 11
     TOG_ACONT AT ROW 7.46 COL 29.13
     FILL-IN_TAKPRIS AT ROW 8.17 COL 8.88 COLON-ALIGNED
     FILL-IN_INKOMST AT ROW 8.17 COL 53.38 COLON-ALIGNED
     TOG_MOMS AT ROW 8.17 COL 68.63
     TOG_MOMS-2 AT ROW 8.17 COL 68.63
     TOG_DIFF AT ROW 9.46 COL 24
     RAD_VISA AT ROW 10.25 COL 2.5 NO-LABEL
     BRW_KOST AT ROW 12.5 COL 1
     BTN_AVB AT ROW 27.75 COL 100.88
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 28.38.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: faktfriatemp T "?" NO-UNDO temp-db faktfriatemp
      TABLE: faktstarttemp T "?" NO-UNDO temp-db faktstarttemp
      TABLE: faktupparbtemp T "?" NO-UNDO temp-db faktupparbtemp
      TABLE: momstemp T "?" NO-UNDO temp-db momstemp
      TABLE: vfaktplantemp T "?" NO-UNDO temp-db vfaktplantemp
      TABLE: ? T "?" NO-UNDO temp-db sumpers
      TABLE: ? T "?" NO-UNDO temp-db kostsum
      TABLE: ? T "?" NO-UNDO temp-db kosttemp
      TABLE: ? T "?" NO-UNDO temp-db gfaktemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-3 ASSIGN
         HIDDEN             = YES
         TITLE              = "Fakturering av löpande räkning arbeten"
         HEIGHT             = 28.63
         WIDTH              = 125
         MAX-HEIGHT         = 28.63
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 28.63
         VIRTUAL-WIDTH      = 125
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WINDOW-3
  NOT-VISIBLE,                                                          */
/* SETTINGS FOR FRAME FRAME-C
                                                                        */
/* BROWSE-TAB BRW_KOST RAD_VISA FRAME-C */
/* SETTINGS FOR BROWSE BRW_KOST IN FRAME FRAME-C
   NO-ENABLE                                                            */
ASSIGN 
       BRW_KOST:HIDDEN  IN FRAME FRAME-C                = TRUE
       BRW_KOST:POPUP-MENU IN FRAME FRAME-C             = MENU POPUP-MENU-BRW_KOST:HANDLE.

/* SETTINGS FOR FILL-IN vfaktplantemp.FAKTNR IN FRAME FRAME-C
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN vfaktplantemp.FAKTNR IN FRAME FRAME-C
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN FILL-IN_FDELNR IN FRAME FRAME-C
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_FDELNR-2 IN FRAME FRAME-C
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_INKOMST IN FRAME FRAME-C
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_TAKPRIS IN FRAME FRAME-C
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_TAKPRIS:HIDDEN IN FRAME FRAME-C           = TRUE.

/* SETTINGS FOR FILL-IN vfaktplantemp.NAMN IN FRAME FRAME-C
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vfaktplantemp.NAMN IN FRAME FRAME-C
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET RAD_VISA IN FRAME FRAME-C
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       RAD_VISA:HIDDEN IN FRAME FRAME-C           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_ACONT IN FRAME FRAME-C
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOG_ACONT:HIDDEN IN FRAME FRAME-C           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_MOMS IN FRAME FRAME-C
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOG_MOMS:HIDDEN IN FRAME FRAME-C           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_MOMS-2 IN FRAME FRAME-C
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOG_MOMS-2:HIDDEN IN FRAME FRAME-C           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_TAK IN FRAME FRAME-C
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOG_TAK:HIDDEN IN FRAME FRAME-C           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-3)
THEN WINDOW-3:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KOST
/* Query rebuild information for BROWSE BRW_KOST
     _TblList          = "Temp-db.kosttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-db.kosttemp.FAKTNR
"kosttemp.FAKTNR" "Ver-nr" ? "character" ? ? ? ? ? ? yes ? no no "13" yes no no "U" "" ""
     _FldNameList[2]   > Temp-db.kosttemp.BENAMNING
"kosttemp.BENAMNING" "Benämning" "X(22)" "character" ? ? ? ? ? ? yes ? no no "17" yes no no "U" "" ""
     _FldNameList[3]   > Temp-db.kosttemp.AONR
"kosttemp.AONR" "Aonr" "X(6)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-db.kosttemp.DELNR
"kosttemp.DELNR" "Del!nr" ">99" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-db.kosttemp.BOKKONTO
"kosttemp.BOKKONTO" "B.för.!konto" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-db.kosttemp.TRAKTKOST
"kosttemp.TRAKTKOST" "Trakt.!kost." "->>>>>>>9" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-db.kosttemp.PERSKOST
"kosttemp.PERSKOST" "Pers.!kost" "->>>>>>>9" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > Temp-db.kosttemp.MASKKOST
"kosttemp.MASKKOST" "Fr.tjänst!kost." "->>>>>>>9" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[9]   > Temp-db.kosttemp.FRTJPAKR
"kosttemp.FRTJPAKR" "Påslag!fr.tjänst" "->>>>>>>9" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[10]   > Temp-db.kosttemp.MTRL
"kosttemp.MTRL" "Mtrl.!kr" "->>>>>>9" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[11]   > Temp-db.kosttemp.MTRLPAKR
"kosttemp.MTRLPAKR" "Påslag!materiel" "->>>>>>>9" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[12]   > Temp-db.kosttemp.OVRKR
"kosttemp.OVRKR" "Övrig!kostnad" "->>>>>>>9" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[13]   > Temp-db.kosttemp.MED
"kosttemp.MED" "Ta!med" "Ja/Nej" "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KOST */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-C
/* Query rebuild information for FRAME FRAME-C
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-C */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_KOST
&Scoped-define SELF-NAME BRW_KOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KOST WINDOW-3
ON ENTRY OF BRW_KOST IN FRAME FRAME-C /* Kostnadsregistrering - "Klicka" på de som ej ska faktureras i MED-fältet */
DO:
   MESSAGE "2" VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KOST WINDOW-3
ON MOUSE-SELECT-DBLCLICK OF BRW_KOST IN FRAME FRAME-C /* Kostnadsregistrering - "Klicka" på de som ej ska faktureras i MED-fältet */
DO:
   RUN medkost_UI (INPUT 1).      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KOST WINDOW-3
ON VALUE-CHANGED OF BRW_KOST IN FRAME FRAME-C /* Kostnadsregistrering - "Klicka" på de som ej ska faktureras i MED-fältet */
DO:
   MESSAGE "1" VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kosttemp.BENAMNING
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kosttemp.BENAMNING BRW_KOST _BROWSE-COLUMN WINDOW-3
ON ENTRY OF kosttemp.BENAMNING IN BROWSE BRW_KOST /* Benämning */
DO:
   DISPLAY kosttemp.BENAMNING WITH BROWSE BRW_KOST.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kosttemp.BENAMNING BRW_KOST _BROWSE-COLUMN WINDOW-3
ON LEAVE OF kosttemp.BENAMNING IN BROWSE BRW_KOST /* Benämning */
DO:
   IF kosttemp.BENAMNING NE INPUT BROWSE BRW_KOST kosttemp.BENAMNING THEN DO:
         kosttemp.BENAMNING = INPUT BROWSE BRW_KOST kosttemp.BENAMNING.   
      
      kosttemp.BENAMNING = INPUT BROWSE BRW_KOST kosttemp.BENAMNING.
      DISPLAY kosttemp.BENAMNING WITH BROWSE BRW_KOST.
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kosttemp.TRAKTKOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kosttemp.TRAKTKOST BRW_KOST _BROWSE-COLUMN WINDOW-3
ON ENTRY OF kosttemp.TRAKTKOST IN BROWSE BRW_KOST /* Trakt.!kost. */
DO:
   DISPLAY kosttemp.TRAKTKOST WITH BROWSE BRW_KOST.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kosttemp.TRAKTKOST BRW_KOST _BROWSE-COLUMN WINDOW-3
ON LEAVE OF kosttemp.TRAKTKOST IN BROWSE BRW_KOST /* Trakt.!kost. */
DO:
   IF kosttemp.TRAKTKOST = INPUT BROWSE BRW_KOST kosttemp.TRAKTKOST THEN musz = musz.
   ELSE DO:
         DEFINE VARIABLE gamla AS DECIMAL NO-UNDO.
         gamla = kosttemp.TRAKTKOST.
         kosttemp.TRAKTKOST = INPUT BROWSE BRW_KOST kosttemp.TRAKTKOST.   
         RUN findg3_UI (INPUT 4, INPUT 0, INPUT 0, INPUT 0,INPUT 0, 
                        INPUT gamla, INPUT kosttemp.TRAKTKOST, INPUT 0, INPUT 0, kosttemp.MED).
    
      DISPLAY kosttemp.TRAKTKOST WITH BROWSE BRW_KOST.
   END.               
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kosttemp.PERSKOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kosttemp.PERSKOST BRW_KOST _BROWSE-COLUMN WINDOW-3
ON ENTRY OF kosttemp.PERSKOST IN BROWSE BRW_KOST /* Pers.!kost */
DO:
   DISPLAY kosttemp.PERSKOST WITH BROWSE BRW_KOST.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kosttemp.PERSKOST BRW_KOST _BROWSE-COLUMN WINDOW-3
ON LEAVE OF kosttemp.PERSKOST IN BROWSE BRW_KOST /* Pers.!kost */
DO:
   IF kosttemp.PERSKOST = INPUT BROWSE BRW_KOST kosttemp.PERSKOST THEN musz = musz.
   ELSE DO: 
         DEFINE VARIABLE gamla AS DECIMAL NO-UNDO.
         gamla = kosttemp.PERSKOST.
         kosttemp.PERSKOST = INPUT BROWSE BRW_KOST kosttemp.PERSKOST.      
         RUN findg3_UI (INPUT 3, INPUT 0, INPUT 0, INPUT 0,INPUT 0, 
                        INPUT gamla, INPUT kosttemp.PERSKOST, INPUT 0, INPUT 0, kosttemp.MED).
      
      DISPLAY kosttemp.PERSKOST WITH BROWSE BRW_KOST.
   END.   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kosttemp.MASKKOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kosttemp.MASKKOST BRW_KOST _BROWSE-COLUMN WINDOW-3
ON ENTRY OF kosttemp.MASKKOST IN BROWSE BRW_KOST /* Fr.tjänst!kost. */
DO:
   DISPLAY kosttemp.MASKKOST WITH BROWSE BRW_KOST.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kosttemp.MASKKOST BRW_KOST _BROWSE-COLUMN WINDOW-3
ON LEAVE OF kosttemp.MASKKOST IN BROWSE BRW_KOST /* Fr.tjänst!kost. */
DO:  
   IF kosttemp.MASKKOST = INPUT BROWSE BRW_KOST kosttemp.MASKKOST THEN musz = musz.
   ELSE DO:
         DEFINE VARIABLE gamla AS DECIMAL NO-UNDO.
         gamla = kosttemp.MASKKOST.
         kosttemp.MASKKOST = INPUT BROWSE BRW_KOST kosttemp.MASKKOST.
         DISPLAY kosttemp.MASKKOST WITH BROWSE BRW_KOST.
         kosttemp.FRTJPAKR = kosttemp.MASKKOST * kundregeltemp.FRTJPA / 100.
         DISPLAY kosttemp.FRTJPAKR WITH BROWSE BRW_KOST.     
         RUN findg3_UI (INPUT 5, INPUT 0, INPUT 0, INPUT 0,INPUT 0, 
                        INPUT gamla, INPUT kosttemp.MASKKOST, INPUT 0, INPUT 0, kosttemp.MED). 
      
      DISPLAY kosttemp.MASKKOST WITH BROWSE BRW_KOST.                     
   END.   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kosttemp.MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kosttemp.MTRL BRW_KOST _BROWSE-COLUMN WINDOW-3
ON ENTRY OF kosttemp.MTRL IN BROWSE BRW_KOST /* Mtrl.!kr */
DO:
   DISPLAY kosttemp.MTRL WITH BROWSE BRW_KOST.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kosttemp.MTRL BRW_KOST _BROWSE-COLUMN WINDOW-3
ON LEAVE OF kosttemp.MTRL IN BROWSE BRW_KOST /* Mtrl.!kr */
DO:
   IF kosttemp.MTRL = INPUT BROWSE BRW_KOST kosttemp.MTRL THEN musz = musz.
   ELSE DO:   
         DEFINE VARIABLE gamla AS DECIMAL NO-UNDO.
         gamla = kosttemp.MTRL.
         kosttemp.MTRL = INPUT BROWSE BRW_KOST kosttemp.MTRL.
         DISPLAY kosttemp.MTRL WITH BROWSE BRW_KOST.
         kosttemp.MTRLPAKR = kosttemp.MTRL * kundregeltemp.MTRLPA / 100.
         DISPLAY kosttemp.MTRLPAKR WITH BROWSE BRW_KOST.
         RUN refreshbrw_UI IN brwproc[7].
         RUN findg3_UI (INPUT 1, INPUT 0, INPUT 0, INPUT 0,INPUT 0, 
                        INPUT gamla, INPUT kosttemp.MTRL, INPUT 0, INPUT 0, kosttemp.MED). 
      
      DISPLAY kosttemp.MTRL WITH BROWSE BRW_KOST.
   END.   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kosttemp.OVRKR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kosttemp.OVRKR BRW_KOST _BROWSE-COLUMN WINDOW-3
ON ENTRY OF kosttemp.OVRKR IN BROWSE BRW_KOST /* Övrig!kostnad */
DO:
   DISPLAY kosttemp.OVRKR WITH BROWSE BRW_KOST.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kosttemp.OVRKR BRW_KOST _BROWSE-COLUMN WINDOW-3
ON LEAVE OF kosttemp.OVRKR IN BROWSE BRW_KOST /* Övrig!kostnad */
DO:
   IF kosttemp.OVRKR = INPUT BROWSE BRW_KOST kosttemp.OVRKR THEN musz = musz.
   ELSE DO:
         DEFINE VARIABLE gamla AS DECIMAL NO-UNDO.
         gamla = kosttemp.OVRKR.
         kosttemp.OVRKR = INPUT BROWSE BRW_KOST kosttemp.OVRKR.
         DISPLAY kosttemp.OVRKR WITH BROWSE BRW_KOST.
         RUN findg3_UI (INPUT 2, INPUT 0, INPUT 0, INPUT 0,INPUT 0, 
                        INPUT gamla, INPUT kosttemp.OVRKR, INPUT 0, INPUT 0, kosttemp.MED).
      
      DISPLAY kosttemp.OVRKR WITH BROWSE BRW_KOST.
   END.             
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kosttemp.MED
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kosttemp.MED BRW_KOST _BROWSE-COLUMN WINDOW-3
ON LEAVE OF kosttemp.MED IN BROWSE BRW_KOST /* Ta!med */
DO:
   /*JA NEJ ?*/
   IF AVAILABLE kosttemp THEN DO:
      IF kosttemp.MED NE INPUT BROWSE BRW_KOST kosttemp.MED THEN DO:       
         RUN medkost_UI (INPUT 2).  
      END. 
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kosttemp.MED BRW_KOST _BROWSE-COLUMN WINDOW-3
ON MOUSE-SELECT-CLICK OF kosttemp.MED IN BROWSE BRW_KOST /* Ta!med */
DO:
   RUN medkost_UI (INPUT 1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-3
ON CHOOSE OF BTN_AVB IN FRAME FRAME-C /* Spara/Avsluta */
DO:   
   slutvar = 0.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SLUTFAKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SLUTFAKT WINDOW-3
ON LEAVE OF FILL-IN_SLUTFAKT IN FRAME FRAME-C /* Slutfakt */
DO:    
   IF FILL-IN_SLUTFAKT = INPUT FILL-IN_SLUTFAKT THEN musz = musz.
   ELSE DO:    
     IF TOG_MOMS = TRUE THEN DO:
         RUN kontokoll_UI.
      END.
      ELSE DO:         
         ASSIGN
         FILL-IN_SLUTFAKT = INPUT FILL-IN_SLUTFAKT.
         RUN slufakt_UI.                      
      END.
   END.   
   DISPLAY FILL-IN_SLUTFAKT WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SLUTFAKT WINDOW-3
ON MOUSE-SELECT-CLICK OF FILL-IN_SLUTFAKT IN FRAME FRAME-C /* Slutfakt */
DO:             
   IF TOG_MOMS = TRUE THEN DO:
      RUN kontokoll_UI.
   END.
   ELSE DO:
      ASSIGN
      FILL-IN_SLUTFAKT = INPUT FILL-IN_SLUTFAKT.
      IF FILL-IN_SLUTFAKT = TRUE THEN FILL-IN_SLUTFAKT = FALSE.
      ELSE FILL-IN_SLUTFAKT = TRUE.
      DISPLAY FILL-IN_SLUTFAKT WITH FRAME {&FRAME-NAME}.      
      RUN slutfakt_UI.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_TAKPRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_TAKPRIS WINDOW-3
ON LEAVE OF FILL-IN_TAKPRIS IN FRAME FRAME-C /* Takpris */
DO:
   IF TOG_MOMS = TRUE THEN DO:
      RUN kontokoll_UI.
   END.
   ELSE DO TRANSACTION:
      FILL-IN_TAKPRIS = INPUT FILL-IN_TAKPRIS.
      vfaktplantemp.TAKPRIS = FILL-IN_TAKPRIS.
      FIND FIRST gfaktemp WHERE gfaktemp.ORDNING = 3 NO-ERROR.
      RUN taktot_UI.
      RUN refreshbrw_UI IN brwproc[6].
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VISA WINDOW-3
ON VALUE-CHANGED OF RAD_VISA IN FRAME FRAME-C
DO:   
   RAD_VISA = INPUT RAD_VISA.         
   MESSAGE RAD_VISA VIEW-AS ALERT-BOX.
   RUN rad_UI.
   RUN openbdyn_UI IN brwproc[7] (INPUT "").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_ACONT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_ACONT WINDOW-3
ON VALUE-CHANGED OF TOG_ACONT IN FRAME FRAME-C /* A-contofakt. */
DO:   
   TOG_ACONT = INPUT TOG_ACONT.
   RUN acont_UI.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_DIFF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_DIFF WINDOW-3
ON VALUE-CHANGED OF TOG_DIFF IN FRAME FRAME-C /* Differentierad moms */
DO:
   TOG_DIFF = INPUT TOG_DIFF.   
   DEBUGGER:SET-BREAK().
   RUN togdiff_UI.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_MOMS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_MOMS WINDOW-3
ON VALUE-CHANGED OF TOG_MOMS IN FRAME FRAME-C /* Kontoberäkningar utförda. */
DO:
   MESSAGE "Vill du ta bort kontoberäkningarna och göra om dem från början ?" 
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Fakturera"
   UPDATE answer1 AS LOGICAL.
   
   
   APPLY "VALUE-CHANGED" TO RAD_VISA.       
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_MOMS-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_MOMS-2 WINDOW-3
ON VALUE-CHANGED OF TOG_MOMS-2 IN FRAME FRAME-C /* Kontoberäkningar utförda. */
DO:
   MESSAGE "Vill du ta bort kontoberäkningarna och göra om dem från början ?" 
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Fakturera"
   UPDATE answer1 AS LOGICAL.
   
   IF answer1 THEN DO:
      TOG_MOMS = FALSE.
      RUN momsbort_UI (TRUE).
      DISPLAY TOG_MOMS WITH FRAME {&FRAME-NAME}.
      TOG_MOMS:HIDDEN = TRUE.      
      IF TOG_MOMS = FALSE THEN DO:
         IF RAD_VISA = 2 AND TOG_DIFF = TRUE THEN DO: 
            
            
         END.
         TOG_ACONT = FALSE.
         TOG_TAK = FALSE.                
         IF valaconttak = TRUE THEN DO:
            FILL-IN_TAKPRIS:HIDDEN = TRUE.
            IF vartyp = 5 THEN DO:
               TOG_TAK = TRUE.
               ENABLE FILL-IN_TAKPRIS TOG_TAK WITH FRAME {&FRAME-NAME}.
               DISPLAY TOG_TAK FILL-IN_TAKPRIS WITH FRAME {&FRAME-NAME}.
            END.
            IF vartyp = 3 THEN DO:
               TOG_ACONT = TRUE.
            END.
            IF FILL-IN_SLUTFAKT = FALSE THEN ENABLE TOG_ACONT WITH FRAME {&FRAME-NAME}.
            ENABLE TOG_TAK WITH FRAME {&FRAME-NAME}.
            DISPLAY TOG_TAK WITH FRAME {&FRAME-NAME}.            
         END.
      END.
   END.
   ELSE DO:
      TOG_MOMS = TRUE.
      DISPLAY TOG_MOMS WITH FRAME {&FRAME-NAME}.      
      
   END.   
   APPLY "VALUE-CHANGED" TO RAD_VISA.       
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_TAK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TAK WINDOW-3
ON VALUE-CHANGED OF TOG_TAK IN FRAME FRAME-C /* Takpris */
DO:
   TOG_TAK = INPUT TOG_TAK.
   IF TOG_TAK = TRUE THEN DO TRANSACTION:      
      ENABLE FILL-IN_TAKPRIS WITH FRAME {&FRAME-NAME}.
      DISPLAY FILL-IN_TAKPRIS WITH FRAME {&FRAME-NAME}.      
      vartyp = 5.      
   END.      
   ELSE DO TRANSACTION:
      FILL-IN_TAKPRIS:HIDDEN = TRUE.
      FIND FIRST gfaktemp WHERE gfaktemp.ORDNING = 3 NO-ERROR.
      gfaktemp.TOTALT = gfaktemp.MTRL + gfaktemp.OVRIG + gfaktemp.ARBKOST + 
                        gfaktemp.OBELOPP + gfaktemp.TRAKT + gfaktemp.RES + 
                        gfaktemp.LONTILL + gfaktemp.KBELOPP.
      IF rundavar = TRUE THEN DO:
         ASSIGN
         gfaktemp.TOTALT = runda(gfaktemp.TOTALT).
      END.
      RUN refreshbrw_UI IN brwproc[6].
      IF valaconttak = TRUE THEN DO:
         IF FILL-IN_SLUTFAKT = FALSE THEN ENABLE TOG_ACONT WITH FRAME {&FRAME-NAME}.      
      END.      
      vartyp = 4.            
   END.   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-3 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:  
   /*detta körs*/      
   APPLY "CHOOSE" TO BTN_AVB IN FRAME {&FRAME-NAME}.
   /*RUN avb_UI.   */
END.
/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:    
    varforetypchar[1] = ">99".
   RUN allstartbrw_UI.
   rad_visa = 1.   
   RUN huvud2_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      LEAVE MAIN-BLOCK.
   END.
  
   RUN skappost_UI.
   OPEN QUERY BRW_GKOST FOR EACH gfaktemp NO-LOCK.
   
   
    {&WINDOW-NAME}:HIDDEN = FALSE.
{&WINDOW-NAME}:MOVE-TO-TOP ().
   APPLY "ENTRY" TO RAD_VISA. 
   ENABLE rad_visa WITH FRAME {&FRAME-NAME}. 
   DISPLAY rad_visa WITH FRAME {&FRAME-NAME}. 
   IF NOT THIS-PROCEDURE:PERSISTENT THEN 
   WAIT-FOR CLOSE OF THIS-PROCEDURE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE acont_UI WINDOW-3 
PROCEDURE acont_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FIND FIRST faktureradtemp WHERE faktureradtemp.FAKTNR = infakplannr AND
   faktureradtemp.FDELNR = FILL-IN_FDELNR NO-LOCK NO-ERROR.
   IF TOG_ACONT = TRUE THEN DO:
      ASSIGN
      FILL-IN_TAKPRIS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_TAK:HIDDEN = TRUE
      TOG_TAK = FALSE.
      FIND FIRST faktureringstyptemp WHERE faktureringstyptemp.FAKTTYPID = faktureradtemp.FAKTTYPID NO-LOCK NO-ERROR.
      IF faktureringstyptemp.TIDIGAREACONTO = TRUE THEN musz = musz.
      ELSE DO:
         
         RUN FATYPVALA.W (INPUT infakplannr,INPUT FILL-IN_FDELNR,INPUT TOG_ACONT,INPUT-OUTPUT TABLE faktureradtemp).
         
      END.
      IF musz = TRUE THEN DO:
         musz = FALSE.
         RETURN.      
      END.
      
      vartyp = 3.
      IF vartyp = 1 OR vartyp = 2 OR vartyp = 3  OR vartyp = 52 OR vartyp = 6 OR vartyp = 7
      THEN TOG_DIFF:HIDDEN = TRUE.
      RUN fastpris_UI.
      FIND FIRST gfaktemp WHERE gfaktemp.ORDNING = 3 NO-ERROR.                 
      gfaktemp.TOTALT = gfaktemp.TOTALT + gfaktemp.OVRIG.
      ASSIGN
      gfaktemp.RES = 0 
      gfaktemp.ARBKOST = 0 
      gfaktemp.LONTILL = 0 
      gfaktemp.MTRL = 0 
      gfaktemp.OBELOPP = 0 
      gfaktemp.TRAKT = 0
      gfaktemp.KBELOPP = 0.
      IF rundavar = TRUE THEN DO:
         ASSIGN
         gfaktemp.TOTALT = runda(gfaktemp.TOTALT).
      END.
     
      OPEN QUERY BRW_GKOST FOR EACH gfaktemp NO-LOCK.
      RUN refreshbrw_UI IN brwproc[6].
   END.      
   ELSE DO:      
      IF valaconttak = TRUE THEN DO:
         TOG_TAK:HIDDEN = FALSE.
         DISPLAY TOG_TAK WITH FRAME {&FRAME-NAME}. 
      END.      
      FIND FIRST faktureringstyptemp WHERE faktureringstyptemp.FAKTTYPID = faktureradtemp.FAKTTYPID NO-LOCK NO-ERROR.
      IF faktureringstyptemp.TIDIGAREACONTO = FALSE THEN musz = musz.
      ELSE DO:
         
         RUN FATYPVALA.W (INPUT infakplannr,INPUT FILL-IN_FDELNR,INPUT TOG_ACONT,INPUT-OUTPUT TABLE faktureradtemp).
         
         IF musz = TRUE THEN DO:
            musz = FALSE.
            RETURN.      
         END.
      END.
      vartyp = 4.
      status-ok = RAD_VISA:DELETE("Faktureringstidpunkter") NO-ERROR.
      IF RAD_VISA = 4 THEN DO:
         RAD_VISA = 1.
         DISPLAY RAD_VISA WITH FRAME {&FRAME-NAME}.         
         RUN visa_UI (INPUT TRUE).
      END.
      FIND FIRST gfaktemp WHERE gfaktemp.ORDNING = 3 NO-ERROR.                 
      gfaktemp.TOTALT = gfaktemp.TOTALT + gfaktemp.OVRIG.
      ASSIGN
      gfaktemp.RES = 0 
      gfaktemp.ARBKOST = 0 
      gfaktemp.LONTILL = 0 
      gfaktemp.MTRL = 0 
      gfaktemp.OBELOPP = 0 
      gfaktemp.TRAKT = 0
      gfaktemp.KBELOPP = 0.      
      
      FOR EACH kosttemp WHERE kosttemp.MED = TRUE BREAK 
      BY kosttemp.AONR BY kosttemp.DELNR:         
         ACCUMULATE 
         kosttemp.PERSKOST (TOTAL BY kosttemp.AONR BY kosttemp.DELNR).
         ACCUMULATE 
         kosttemp.TRAKTKOST (TOTAL BY kosttemp.AONR BY kosttemp.DELNR).
         ACCUMULATE 
         kosttemp.FRTJPAKR (TOTAL BY kosttemp.AONR BY kosttemp.DELNR). 
         ACCUMULATE 
         kosttemp.MTRLPAKR (TOTAL BY kosttemp.AONR BY kosttemp.DELNR).
         ACCUMULATE 
         kosttemp.MASKKOST (TOTAL BY kosttemp.AONR BY kosttemp.DELNR). 
         ACCUMULATE 
         kosttemp.MTRL (TOTAL BY kosttemp.AONR BY kosttemp.DELNR).
         ACCUMULATE             
         kosttemp.OVRKR (TOTAL BY kosttemp.AONR BY kosttemp.DELNR).      
         IF LAST-OF(kosttemp.DELNR) THEN DO:            
            ASSIGN
            gfaktemp.MTRL = gfaktemp.MTRL + 
            (ACCUM TOTAL BY kosttemp.DELNR kosttemp.MTRLPAKR) + 
            (ACCUM TOTAL BY kosttemp.DELNR kosttemp.MTRL)
            gfaktemp.OVRIG = gfaktemp.OVRIG + 
            (ACCUM TOTAL BY kosttemp.DELNR kosttemp.OVRKR)
            gfaktemp.ARBKOST = gfaktemp.ARBKOST + 
            (ACCUM TOTAL BY kosttemp.DELNR kosttemp.PERSKOST) 
            gfaktemp.TRAKT = gfaktemp.TRAKT + 
            (ACCUM TOTAL BY kosttemp.DELNR kosttemp.TRAKTKOST)  
            gfaktemp.KBELOPP = gfaktemp.KBELOPP + 
            (ACCUM TOTAL BY kosttemp.DELNR kosttemp.FRTJPAKR) + 
            (ACCUM TOTAL BY kosttemp.DELNR kosttemp.MASKKOST).
         END.
      END.
      FIND FIRST gfaktemp WHERE gfaktemp.ORDNING = 3 NO-ERROR.
      gfaktemp.TOTALT = gfaktemp.RES + 
                        gfaktemp.ARBKOST + gfaktemp.LONTILL + gfaktemp.MTRL + 
                        gfaktemp.OBELOPP + gfaktemp.OVRIG + gfaktemp.TRAKT + 
                        gfaktemp.KBELOPP.
      IF rundavar = TRUE THEN DO:
         ASSIGN
         gfaktemp.TOTALT = runda(gfaktemp.TOTALT).
      END.
      RUN refreshbrw_UI IN brwproc[6].
      RAD_VISA = 5. 
      DISPLAY RAD_VISA WITH FRAME {&FRAME-NAME}.
      APPLY "VALUE-CHANGED" TO RAD_VISA.
   END.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-3 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[7] 
      (INPUT BRW_KOST:HANDLE IN FRAME {&FRAME-NAME}).
   RUN mouseselclick_UI IN brwproc[7] (INPUT FALSE). /*Disable APPLY "VALUE-CHANGED" TO browser*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avb_UI WINDOW-3 
PROCEDURE avb_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   
   DEFINE VARIABLE i AS INTEGER NO-UNDO INITIAL 1.
DO WHILE i LE 25:
   IF VALID-HANDLE(brwproc[i]) THEN DELETE PROCEDURE brwproc[i] NO-ERROR.
   brwproc[i] = ?.
   i = i + 1.
END.
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE befat_UI WINDOW-3 
PROCEDURE befat_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE berraknatid_UI WINDOW-3 
PROCEDURE berraknatid_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bertidkost_UI WINDOW-3 
PROCEDURE bertidkost_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER medvar AS LOGICAL NO-UNDO.
   
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changefgcolor_UI WINDOW-3 
PROCEDURE changefgcolor_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   DEFINE INPUT PARAMETER colname AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER colh AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER bufh AS HANDLE NO-UNDO.   
   IF bufh:BUFFER-VALUE = FALSE THEN DO:
      colh:FGCOLOR = 12.      
   END.                                       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-3  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-3)
  THEN DELETE WIDGET WINDOW-3.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-3  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY FILL-IN_FDELNR-2 FILL-IN_FDELNR FILL-IN_SLUTFAKT FILL-IN_INKOMST 
          TOG_DIFF 
      WITH FRAME FRAME-C IN WINDOW WINDOW-3.
  IF AVAILABLE vfaktplantemp THEN 
    DISPLAY vfaktplantemp.FAKTNR vfaktplantemp.FAKTNR vfaktplantemp.NAMN 
          vfaktplantemp.NAMN 
      WITH FRAME FRAME-C IN WINDOW WINDOW-3.
  ENABLE FILL-IN_SLUTFAKT TOG_DIFF BTN_AVB 
      WITH FRAME FRAME-C IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-C}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fakstartfak_UI WINDOW-3 
PROCEDURE fakstartfak_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fastpris_UI WINDOW-3 
PROCEDURE fastpris_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
         
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE findg3_UI WINDOW-3 
PROCEDURE findg3_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER typvar AS DECIMAL NO-UNDO.
   DEFINE INPUT PARAMETER gbelopp AS DECIMAL NO-UNDO.
   DEFINE INPUT PARAMETER nybelopp AS DECIMAL NO-UNDO.
   DEFINE INPUT PARAMETER gobelopp AS DECIMAL NO-UNDO.
   DEFINE INPUT PARAMETER nyobelopp AS DECIMAL NO-UNDO.
   DEFINE INPUT PARAMETER gkbelopp AS DECIMAL NO-UNDO.
   DEFINE INPUT PARAMETER nykbelopp AS DECIMAL NO-UNDO.
   DEFINE INPUT PARAMETER gres AS DECIMAL NO-UNDO.
   DEFINE INPUT PARAMETER nyres AS DECIMAL NO-UNDO.
   DEFINE INPUT PARAMETER medvar AS LOGICAL NO-UNDO.
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fripris_UI WINDOW-3 
PROCEDURE fripris_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*endast fria kompletteringar*/    
   
   RUN open_UI.
   RAD_VISA = 3.
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gladda_UI WINDOW-3 
PROCEDURE gladda_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER val AS INTEGER NO-UNDO.
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gomafri_UI WINDOW-3 
PROCEDURE gomafri_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER gomavar AS LOGICAL NO-UNDO.
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gtid_UI WINDOW-3 
PROCEDURE gtid_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud2_UI WINDOW-3 
PROCEDURE huvud2_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   
   RUN lop_UI.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud_UI WINDOW-3 
PROCEDURE huvud_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE klar_UI WINDOW-3 
PROCEDURE klar_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   
             
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kontokoll_UI WINDOW-3 
PROCEDURE kontokoll_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF TOG_MOMS = TRUE THEN DO:
      MESSAGE "Kontoberäkningar utförda. Inga ändringar kan göras!" VIEW-AS ALERT-BOX.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kostreg_UI WINDOW-3 
PROCEDURE kostreg_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/         
   FIND FIRST gfaktemp WHERE gfaktemp.ORDNING = 3 NO-ERROR.
   IF NOT AVAILABLE gfaktemp THEN CREATE gfaktemp.
   ASSIGN
   FILL-IN-TOMDAT = TODAY
   gfaktemp.TYPTEXT = "Denna faktura" 
   gfaktemp.ORDNING = 3.
   IF Guru.Konstanter:appcon THEN DO:
      RUN FAKOSAPPA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT infakplannr,INPUT ?,INPUT ?, INPUT FILL-IN-TOMDAT, INPUT-OUTPUT FILL-IN_INKOMST, OUTPUT TABLE kosttemp).
   END.
   ELSE DO:
      RUN FAKOSAPPA.P 
      (INPUT infakplannr,INPUT ?,INPUT ?, INPUT FILL-IN-TOMDAT, INPUT-OUTPUT FILL-IN_INKOMST,OUTPUT TABLE kosttemp).
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lop_UI WINDOW-3 
PROCEDURE lop_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN                      
   RAD_VISA = 3.      
   RUN open_UI.   
   RUN skappost_UI.  
   FIND FIRST kosttemp NO-LOCK NO-ERROR.
   IF AVAILABLE kosttemp THEN DO:
      RAD_VISA = 2.
      RUN openkost_UI (INPUT 2).
   END.            
   RUN openfalt_UI. 
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE medkost_UI WINDOW-3 
PROCEDURE medkost_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER medraknare AS INTEGER NO-UNDO.
   END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE momsbort_UI WINDOW-3 
PROCEDURE momsbort_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE INPUT PARAMETER fribortvar AS LOGICAL NO-UNDO.
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE moms_UI WINDOW-3 
PROCEDURE moms_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openfalt_UI WINDOW-3 
PROCEDURE openfalt_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE openkost_UI WINDOW-3 
PROCEDURE openkost_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   /*HÄMTAR SPARADE KOSTREG OCH BERÄKNAR KOSTNAD*/
      
   DEFINE INPUT PARAMETER korvar AS INTEGER NO-UNDO.
   FIND FIRST gfaktemp WHERE gfaktemp.ORDNING = 3 NO-ERROR.
   IF NOT AVAILABLE gfaktemp THEN CREATE gfaktemp.
   ASSIGN
   gfaktemp.TYPTEXT = "Denna faktura" 
   gfaktemp.ORDNING = 3.
   IF korvar = 2 THEN DO:
      RUN openbdyn_UI IN brwproc[7] (INPUT "").
      GET FIRST BRW_KOST NO-LOCK.
      DO WHILE AVAILABLE(kosttemp):      
         IF kosttemp.MED = TRUE THEN DO:         
            ASSIGN
            gfaktemp.MTRL = gfaktemp.MTRL + kosttemp.MTRLPAKR + kosttemp.MTRL
            gfaktemp.OVRIG = gfaktemp.OVRIG + kosttemp.OVRKR
            gfaktemp.ARBKOST = gfaktemp.ARBKOST + kosttemp.PERSKOST 
            gfaktemp.TRAKT = gfaktemp.TRAKT + kosttemp.TRAKTKOST  
            gfaktemp.KBELOPP = gfaktemp.KBELOPP + kosttemp.FRTJPAKR + kosttemp.MASKKOST.                                     
         END.      
         GET NEXT BRW_KOST NO-LOCK.       
      END.                
   END. 
   RUN berraknatid_UI.
   ENABLE BRW_KOST WITH FRAME {&FRAME-NAME}.  
   RUN openbdyn_UI IN brwproc[7] (INPUT "").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open_UI WINDOW-3 
PROCEDURE open_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   
           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE orderh_UI WINDOW-3 
PROCEDURE orderh_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   DEFINE VARIABLE FILL-IN_SLUTFAKT AS LOGICAL NO-UNDO.
   DEFINE VARIABLE TOG_ACONT AS LOGICAL NO-UNDO.
    

   IF musz = TRUE THEN DO:
      RETURN.         
   END.             
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE overberbyt2_UI WINDOW-3 
PROCEDURE overberbyt2_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE overberbyt_UI WINDOW-3 
PROCEDURE overberbyt_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   

     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE overegna_UI WINDOW-3 
PROCEDURE overegna_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
     
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rad_UI WINDOW-3 
PROCEDURE rad_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   IF RAD_VISA = 4 THEN RUN visa_UI (INPUT FALSE).
   ELSE IF RAD_VISA = 6 THEN RUN visa_UI (INPUT FALSE).
   ELSE RUN visa_UI (INPUT TRUE).   
   IF vartyp = 1 OR vartyp = 2 OR vartyp = 3 OR vartyp = 52 OR vartyp = 6 OR vartyp = 7 THEN DO:
      
      
   END.
   ELSE IF RAD_VISA NE 2 THEN DO:
      
   END.
   ELSE DO:
      
   END.
   ENABLE brw_kost WITH FRAME {&FRAME-NAME}. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowfri_UI WINDOW-3 
PROCEDURE rowfri_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF faktfriatemp.TYP = "" THEN DO:
      
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skappost_UI WINDOW-3 
PROCEDURE skappost_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/               
   /*HÄMTA KOSTNADSREGISTRERINGAR*/
   RUN kostreg_UI.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE slutfakt_UI WINDOW-3 
PROCEDURE slutfakt_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/               
   
   
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sortvar_UI WINDOW-3 
PROCEDURE sortvar_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/      

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spara_UI WINDOW-3 
PROCEDURE spara_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER skarp AS INTEGER NO-UNDO.
   
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE stid_UI WINDOW-3 
PROCEDURE stid_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*SPARADE FAKTTIDER*/
      
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE storlek_UI WINDOW-3 
PROCEDURE storlek_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sumtimmar_UI WINDOW-3 
PROCEDURE sumtimmar_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE takpris_UI WINDOW-3 
PROCEDURE takpris_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE taktot_UI WINDOW-3 
PROCEDURE taktot_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
          
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tidegna_UI WINDOW-3 
PROCEDURE tidegna_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
     

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tidreg_UI WINDOW-3 
PROCEDURE tidreg_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tidvisa_UI WINDOW-3 
PROCEDURE tidvisa_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/            
   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tladda_UI WINDOW-3 
PROCEDURE tladda_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   DEFINE INPUT PARAMETER visatotvar AS INTEGER NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE togdiff_UI WINDOW-3 
PROCEDURE togdiff_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visa_UI WINDOW-3 
PROCEDURE visa_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/     
   
    
   DEFINE INPUT PARAMETER fraga AS LOGICAL NO-UNDO. 
   ASSIGN
   BRW_KOST:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.  
   
   IF RAD_VISA = 1 THEN DO:
      
   END.    
   ELSE IF RAD_VISA = 2 THEN DO:                   
      ASSIGN
      BRW_KOST:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.      
   END.
   ELSE IF RAD_VISA = 3 THEN DO:                  
      
   END.   
   ELSE IF RAD_VISA = 4 THEN DO:                  
   END.
   ELSE IF RAD_VISA = 5 THEN DO:                        
   
   END.
   ELSE IF RAD_VISA = 6 THEN DO:                        
   
   END.
   DISPLAY RAD_VISA WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION klockan100 WINDOW-3 
FUNCTION klockan100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL ):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION klockan60 WINDOW-3 
FUNCTION klockan60 RETURNS DECIMAL
  ( INPUT ber100 AS DECIMAL ):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  RETURN TRUNCATE(ber100,0) + ((ber100 - TRUNCATE(ber100,0)) / 100) * 60 . 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION runda WINDOW-3 
FUNCTION runda RETURNS DECIMAL
  ( INPUT varedin AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN ROUND(varedin,0).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

