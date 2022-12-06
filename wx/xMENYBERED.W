&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-1


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/05/02 - 12:41 pm

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
DEFINE TEMP-TABLE berintemp NO-UNDO
   FIELD AONR             AS CHARACTER                           
   FIELD DELNR            AS INTEGER                            
   FIELD OMRADE           AS CHARACTER                         
   FIELD ORT              AS CHARACTER                            
   FIELD BESTID           AS CHARACTER                         
   FIELD AONRAVDATUM      AS DATE                         
   FIELD STARTDATUM       AS DATE                          
   FIELD AONRREC          AS RECID                            
   FIELD FAKTTYP          AS CHARACTER                        
   FIELD FAKTNR           AS INTEGER                           
   FIELD ARBANSVARIG      AS CHARACTER                    
   FIELD BEREDARE         AS CHARACTER                       
   FIELD PROJEKTOR        AS CHARACTER                      
   FIELD TABORT           AS LOGICAL                           
   FIELD FASTAAONR        AS LOGICAL                        
   FIELD TRAKTAMENTE      AS INTEGER                      
   FIELD PRISTYP          AS CHARACTER
   FIELD UTRYCKNING       AS LOGICAL
   FIELD AVDELNINGNR      AS INTEGER
   FIELD ANLNR            AS CHARACTER
   FIELD ANM              AS CHARACTER EXTENT 6
   FIELD ANVANDARE        AS CHARACTER  
   FIELD ARBARTKOD        AS INTEGER  
   FIELD ARBBESKED        AS LOGICAL 
   FIELD ARBUPPG          AS CHARACTER EXTENT 2 
   FIELD ARTAL            AS INTEGER 
   FIELD AUTOREG          AS LOGICAL     
   FIELD BETNR            AS INTEGER   
   FIELD ELVOMRKOD        AS INTEGER 
   FIELD FASTKALK         AS LOGICAL  
   FIELD KALKNR           AS INTEGER 
   FIELD PKOD             AS INTEGER 
   FIELD PLANNR           AS CHARACTER   
   FIELD SLUTDAG          AS CHARACTER 
   FIELD SLUTVNR          AS INTEGER 
   FIELD STARTDAG         AS CHARACTER   
   FIELD STARTVNR         AS INTEGER   
   FIELD UTFARDAT         AS CHARACTER   
   FIELD JUDID            AS CHARACTER
   FIELD VIJUDID          AS CHARACTER
   INDEX AONR IS PRIMARY AONR DELNR
   INDEX OMRADE OMRADE AONR DELNR
   INDEX ORT ORT AONR DELNR
   INDEX TABORT TABORT. 

DEFINE TEMP-TABLE uppvaltemp  NO-UNDO
   FIELD MANUPPDEL AS LOGICAL
   FIELD VALDLISTA AS CHARACTER
   FIELD VISPERAR AS LOGICAL
   FIELD VISGODKANDA AS LOGICAL
   FIELD DELNRKOLL AS LOGICAL
   FIELD ENDBEST AS LOGICAL
   FIELD STARTDATUM AS DATE
   FIELD SLUTDATUM AS DATE
   FIELD AVSLUTSTART AS DATE
   FIELD AVSLUTSLUT AS DATE
   FIELD TILLFALLFAST AS INTEGER
   FIELD BESTID AS CHARACTER
   FIELD BESTNAMN AS CHARACTER
   FIELD OMRNAMN AS CHARACTER
   FIELD OMRADE AS CHARACTER
   FIELD AVDNAMN AS CHARACTER
   FIELD AVDNR   AS CHARACTER
   FIELD FAKTTYP AS CHARACTER
   FIELD PAAV AS INTEGER
   FIELD ARBANSVARIG AS CHARACTER
   FIELD BEREDARE AS CHARACTER
   FIELD PROJEKTOR AS CHARACTER.

DEFINE {&NEW} {&SHARED} TEMP-TABLE utsokaonr NO-UNDO
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD OMRADE AS CHARACTER
   FIELD ORT AS CHARACTER
   FIELD BESTID AS CHARACTER
   FIELD AONRAVDATUM AS DATE
   FIELD STARTDATUM AS DATE
   FIELD AONRREC AS RECID
   FIELD FAKTTYP AS CHARACTER
   FIELD FAKTNR AS INTEGER
   FIELD ARBANSVARIG AS CHARACTER
   FIELD BEREDARE AS CHARACTER
   FIELD PROJEKTOR AS CHARACTER
   FIELD TABORT AS LOGICAL
   FIELD FASTAAONR AS LOGICAL
   FIELD TRAKTAMENTE AS INTEGER
   FIELD PRISTYP AS CHARACTER
   FIELD UTRYCKNING AS LOGICAL
   FIELD AVDELNINGNR      AS INTEGER
   FIELD ANLNR            AS CHARACTER
   FIELD ANM              AS CHARACTER EXTENT 6
   FIELD ANVANDARE        AS CHARACTER
   FIELD ARBARTKOD        AS INTEGER
   FIELD ARBBESKED        AS LOGICAL
   FIELD ARBUPPG          AS CHARACTER EXTENT 2
   FIELD ARTAL            AS INTEGER
   FIELD AUTOREG          AS LOGICAL
   FIELD BETNR            AS INTEGER
   FIELD ELVOMRKOD        AS INTEGER
   FIELD FASTKALK         AS LOGICAL
   FIELD KALKNR           AS INTEGER
   FIELD PKOD             AS INTEGER
   FIELD PLANNR           AS CHARACTER
   FIELD SLUTDAG          AS CHARACTER
   FIELD SLUTVNR          AS INTEGER
   FIELD STARTDAG         AS CHARACTER
   FIELD STARTVNR         AS INTEGER
   FIELD UTFARDAT         AS CHARACTER
   FIELD JUDID            AS CHARACTER
   FIELD VIJUDID          AS CHARACTER
   INDEX AONR IS PRIMARY AONR DELNR
   INDEX OMRADE OMRADE AONR DELNR
   INDEX ORT ORT AONR DELNR
   INDEX TABORT TABORT.
DEFINE {&NEW} {&SHARED} TEMP-TABLE valdaao NO-UNDO  LIKE utsokaonr.
DEFINE TEMP-TABLE evaldaao NO-UNDO  LIKE utsokaonr.

DEFINE VARIABLE franvart AS INTEGER NO-UNDO.
franvart = 1.

/* Local Variable Definitions ---                                       */  
DEFINE VARIABLE framesizeh AS HANDLE NO-UNDO.
/*FRAMSIZETEMP.I*/
DEFINE TEMP-TABLE frametemp NO-UNDO
   FIELD ORDNING AS INTEGER
   FIELD FRMHAND AS HANDLE
   FIELD FHOJD AS INTEGER
   FIELD FBREDD AS INTEGER
   FIELD MFHOJD AS INTEGER
   FIELD MFBREDD AS INTEGER
   FIELD FRAMENAMN AS CHARACTER
   INDEX ORDNING ORDNING.

/* DEFINE VARIABLE framesizedh AS HANDLE NO-UNDO. */
DEFINE SHARED VARIABLE tth AS HANDLE NO-UNDO.
/*DEFINE SHARED VARIABLE hpApi AS HANDLE NO-UNDO.*/
DEFINE SHARED VARIABLE hpWinFunc AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE retvalkoll AS LOGICAL NO-UNDO.
DEFINE VARIABLE hcur AS INTEGER NO-UNDO.
DEFINE VARIABLE retval AS INTEGER NO-UNDO.
DEFINE VARIABLE brwproc AS HANDLE EXTENT 25 NO-UNDO.
DEFINE VARIABLE handproc AS HANDLE EXTENT 25 NO-UNDO.
DEFINE VARIABLE apphandass AS HANDLE NO-UNDO.
DEFINE VARIABLE apphandklar AS HANDLE NO-UNDO.
DEFINE VARIABLE appprogok AS LOGICAL NO-UNDO.
&GLOBAL-DEFINE LEFT-BROWSE 1
&GLOBAL-DEFINE RIGHT-BROWSE 2
&GLOBAL-DEFINE ARROWS 3
DEFINE VARIABLE tthandle AS HANDLE NO-UNDO.
DEFINE VARIABLE ReturnValue AS INTEGER.
DEFINE {&NEW} {&SHARED} TEMP-TABLE sparaladdatemp
   FIELD SLVAL      AS INTEGER
   FIELD GLOBANV    AS CHARACTER 
   FIELD BENAMNING  AS CHARACTER 
   FIELD TABVAL     AS CHARACTER 
   FIELD FALTVAL    AS LOGICAL 
   FIELD FALTVALAO  AS CHARACTER 
   FIELD FALTVALDEL AS CHARACTER 
   FIELD FALTVALDATE AS CHARACTER 
   INDEX FALTVALAO IS PRIMARY FALTVALAO.

   &Scoped-define NEW NEW

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
DEFINE {&NEW} SHARED VARIABLE plusaonr AS CHARACTER NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE plusdnr AS INTEGER NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE plusrec AS RECID  NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE plustidrec AS RECID  NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE plustid AS DECIMAL NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE plusdval AS LOGICAL NO-UNDO.
DEFINE {&NEW} SHARED VARIABLE succelval AS LOGICAL NO-UNDO. 
/*VARFORETYPVAR.I*/
DEFINE {&NEW} SHARED VARIABLE varforetypval AS INTEGER EXTENT 100 NO-UNDO.     
DEFINE {&NEW} SHARED VARIABLE varforetypchar AS CHARACTER EXTENT 100 NO-UNDO.  
  

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


&Scoped-define NEW                
DEFINE NEW SHARED VARIABLE avbryt AS LOGICAL NO-UNDO.                           
DEFINE NEW SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE kalkrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE kalkrec2 AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE karlrec AS RECID NO-UNDO. 
DEFINE NEW SHARED VARIABLE aoplan AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE valbernr AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE valomrade AS CHARACTER NO-UNDO. 
DEFINE NEW SHARED VARIABLE katvar AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE kalkvar AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE ejanv AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE nyvar AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE datvar AS DATE NO-UNDO.
DEFINE NEW SHARED VARIABLE skapamtrl AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE lista AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE inkop AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE NEW SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE NEW SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE aonrrec2 AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE berstopp AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO. 
DEFINE VARIABLE fellog AS LOGICAL NO-UNDO.
DEFINE VARIABLE leftbrwakt AS LOGICAL NO-UNDO.
DEFINE VARIABLE valaonr2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE valomrade2 AS CHARACTER NO-UNDO.
DEFINE NEW SHARED TEMP-TABLE omrtemp NO-UNDO
   FIELD ANVANDARE AS CHARACTER
   FIELD PANDRA AS LOGICAL
   FIELD AVDELNINGNR AS INTEGER
   FIELD OMRADE AS CHARACTER
   FIELD NAMN AS CHARACTER
   INDEX OMR IS PRIMARY OMRADE
   INDEX OMRNAMN NAMN.
DEFINE VARIABLE statusomr AS LOGICAL NO-UNDO.
statusomr = TRUE.

/*SOKDEF.I*/
DEFINE TEMP-TABLE soktemp NO-UNDO
   FIELD SOKVAL AS INTEGER 
   FIELD SOKCHAR AS CHARACTER EXTENT 5
   FIELD SOKDATE AS DATE EXTENT 5
   FIELD SOKINT AS INTEGER EXTENT 5
   FIELD SOKDECI AS DECIMAL EXTENT 5
   FIELD SOKLOG AS LOGICAL EXTENT 5.

DEFINE TEMP-TABLE valsoktemp NO-UNDO
   FIELD SOKVAL AS INTEGER 
   FIELD SOKCHAR AS CHARACTER EXTENT 10
   FIELD SOKDATE AS DATE EXTENT 10
   FIELD SOKINT AS INTEGER EXTENT 10
   FIELD SOKDECI AS DECIMAL EXTENT 10
   FIELD SOKLOG AS LOGICAL EXTENT 10.

  


&Scoped-define NEW NEW     
&Scoped-define SHARED SHARED

DEFINE {&NEW} {&SHARED} TEMP-TABLE urberedningtemp NO-UNDO
   FIELD AKTIV          AS LOGICAL
   FIELD ANVANDARE      AS CHARACTER
   FIELD AONR           AS CHARACTER
   FIELD BENAMNING      AS CHARACTER
   FIELD BERAONR        AS CHARACTER
   FIELD BERNR          AS INTEGER
   FIELD DELNR          AS INTEGER
   FIELD KALKYL         AS LOGICAL
   FIELD KATAR          AS INTEGER
   FIELD OMRADE         AS CHARACTER
   INDEX BERNR IS PRIMARY BERNR
   INDEX OMRADE OMRADE BERNR AONR DELNR
   INDEX AONR AONR DELNR BERNR
   INDEX DELNR DELNR AONR
   INDEX AKTIV AKTIV BERNR.

DEFINE {&NEW} {&SHARED} TEMP-TABLE valberedningtemp NO-UNDO LIKE urberedningtemp. 



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_URBER

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES urberedningtemp valberedningtemp

/* Definitions for BROWSE BRW_URBER                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_URBER urberedningtemp.OMRADE ~
urberedningtemp.BERNR urberedningtemp.BENAMNING urberedningtemp.AONR ~
urberedningtemp.DELNR urberedningtemp.AKTIV urberedningtemp.ANVANDARE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_URBER urberedningtemp.OMRADE 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_URBER urberedningtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_URBER urberedningtemp
&Scoped-define OPEN-QUERY-BRW_URBER OPEN QUERY BRW_URBER FOR EACH urberedningtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_URBER urberedningtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_URBER urberedningtemp


/* Definitions for BROWSE BRW_VBER                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_VBER valberedningtemp.OMRADE ~
valberedningtemp.BERNR valberedningtemp.BENAMNING valberedningtemp.AONR ~
valberedningtemp.DELNR valberedningtemp.AKTIV valberedningtemp.ANVANDARE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VBER valberedningtemp.OMRADE 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_VBER valberedningtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_VBER valberedningtemp
&Scoped-define OPEN-QUERY-BRW_VBER OPEN QUERY BRW_VBER FOR EACH valberedningtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VBER valberedningtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VBER valberedningtemp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS TOG_AKT TOG_INAKT CMB_OMR CMB_UTF BTN_HAMT ~
BRW_URBER BRW_VBER BTN_ATG BTN_ALLOVER BTN_KOPI BTN_OVER BTN_BACK ~
BTN_ALLBACK FILL-IN_AONR FILL-IN-BERNR FILL-IN-BEN FILL-IN-HBERNR BTN_AVB ~
RECT-20 RECT-22 RECT-5 RECT-53 
&Scoped-Define DISPLAYED-OBJECTS TOG_AKT TOG_INAKT CMB_OMR CMB_UTF ~
FILL-IN_AONR FILL-IN-BERNR FILL-IN-BEN FILL-IN-HBERNR FILL-IN-AOTEXT ~
FILL-IN-SOKTEXT FILL-IN-HTEXT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD brwval WINDOW-1 
FUNCTION brwval RETURNS LOGICAL
  (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD felkoll WINDOW-1 
FUNCTION felkoll RETURNS LOGICAL
  ( INPUT felmed AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ADM 
     LABEL "Administration" 
     SIZE 18 BY 1.5.

DEFINE BUTTON BTN_ALLBACK 
     IMAGE-UP FILE "BILDER\rewind-u":U
     LABEL "Alla aonr i listan":L 
     SIZE 5.5 BY 1.75 TOOLTIP "Alla valda aonr tas bort från vallistan"
     .

DEFINE BUTTON BTN_ALLOVER 
     IMAGE-UP FILE "BILDER\forwrd-u":U
     LABEL "Alla aonr i listan":L 
     SIZE 5.5 BY 1.75 TOOLTIP "Alla aonr väljs"
     .

DEFINE BUTTON BTN_ATG 
     LABEL "Åtgärder" 
     SIZE 18 BY 1.5.

DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 18 BY 1.5.

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 5.5 BY 1.75 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort":L 
     SIZE 10.5 BY 1.5.

DEFINE BUTTON BTN_EXP 
     LABEL "Export" 
     SIZE 18 BY 1.5.

DEFINE BUTTON BTN_HAMT 
     LABEL "Hämta och visa urval" 
     SIZE 22 BY 1.13 TOOLTIP "Dina val ovan avgör vilka aonr du får i listan nedan.".

DEFINE BUTTON BTN_IMP 
     LABEL "Import" 
     SIZE 18 BY 1.5.

DEFINE BUTTON BTN_INAKTIV 
     LABEL "Aktiv/inaktiv":L 
     SIZE 18 BY 1.5.

DEFINE BUTTON BTN_INK 
     LABEL "Inköp" 
     SIZE 18 BY 1.5.

DEFINE BUTTON BTN_KALK 
     LABEL "Bereda":L 
     SIZE 18 BY 1.5.

DEFINE BUTTON BTN_KOPI 
     LABEL "Kopiera" 
     SIZE 18 BY 1.5.

DEFINE BUTTON BTN_LAS 
     LABEL "Låsta beredningar" 
     SIZE 18 BY 1.5.

DEFINE BUTTON BTN_LIST 
     LABEL "Listor" 
     SIZE 18 BY 1.5.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 10.5 BY 1.5.

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 5.5 BY 1.75 TOOLTIP "Markerade väljs".

DEFINE BUTTON BTN_UPP 
     LABEL "Beredningshuvud":L 
     SIZE 18 BY 1.5.

DEFINE VARIABLE CMB_OMR AS CHARACTER FORMAT "X(256)":U INITIAL ? 
     LABEL "Område" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_UTF AS CHARACTER FORMAT "X(256)":U 
     LABEL "Utfärdare" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AOTEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Gör urval av beredning:" 
      VIEW-AS TEXT 
     SIZE 26.5 BY 1.5
      NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "x(40)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 17.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BERNR AS INTEGER FORMAT ">,>>>,>>9" INITIAL 0 
     LABEL "Beredningsnr" 
     VIEW-AS FILL-IN 
     SIZE 11.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-HBERNR AS INTEGER FORMAT "->>>>>>>>9" INITIAL 0 
     LABEL "Beredningsnr" 
     VIEW-AS FILL-IN 
     SIZE 10.5 BY 1 TOOLTIP "Tryck RETURN här för välja enstaka beredningar.".

DEFINE VARIABLE FILL-IN-HTEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Hämta beredning:" 
      VIEW-AS TEXT 
     SIZE 17 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN-SOKTEXT AS CHARACTER FORMAT "X(256)":U INITIAL "Sök i urval:" 
      VIEW-AS TEXT 
     SIZE 12.5 BY .67 NO-UNDO.

DEFINE VARIABLE FILL-IN_AONR AS CHARACTER FORMAT "X(6)" INITIAL "0" 
     LABEL "Aonr" 
     VIEW-AS FILL-IN 
     SIZE 11.5 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 33.13 BY 2.83
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 61 BY 4.58
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 20 BY 27
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-53
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 104 BY 27.

DEFINE VARIABLE TOG_AKT AS LOGICAL INITIAL yes 
     LABEL "Aktiva":L 
     VIEW-AS TOGGLE-BOX
     SIZE 10.75 BY 1 NO-UNDO.

DEFINE VARIABLE TOG_INAKT AS LOGICAL INITIAL no 
     LABEL "Inaktiva":L 
     VIEW-AS TOGGLE-BOX
     SIZE 11.63 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_URBER FOR 
      urberedningtemp SCROLLING.

DEFINE QUERY BRW_VBER FOR 
      valberedningtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_URBER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_URBER WINDOW-1 _STRUCTURED
  QUERY BRW_URBER NO-LOCK DISPLAY
      urberedningtemp.OMRADE FORMAT "x(6)":U
      urberedningtemp.BERNR COLUMN-LABEL "Ber.nr" FORMAT "->,>>>,>>9":U
            WIDTH 7 COLUMN-FGCOLOR 5 LABEL-FGCOLOR 5
      urberedningtemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(30)":U
            WIDTH 26
      urberedningtemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
            COLUMN-FGCOLOR 2 LABEL-FGCOLOR 2
      urberedningtemp.DELNR COLUMN-LABEL "Del!nr" FORMAT ">99":U
            COLUMN-FGCOLOR 2 LABEL-FGCOLOR 2
      urberedningtemp.AKTIV COLUMN-LABEL "Aktiv/!Inaktiv" FORMAT "Aktiv/Inaktiv":U
      urberedningtemp.ANVANDARE COLUMN-LABEL "Utfärdare" FORMAT "x(12)":U
  ENABLE
      urberedningtemp.OMRADE
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS MULTIPLE SIZE 61 BY 12.75
         TITLE "Urvalsresultat".

DEFINE BROWSE BRW_VBER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VBER WINDOW-1 _STRUCTURED
  QUERY BRW_VBER NO-LOCK DISPLAY
      valberedningtemp.OMRADE FORMAT "x(6)":U WIDTH 6.5
      valberedningtemp.BERNR COLUMN-LABEL "Ber.nr" FORMAT "->,>>>,>>9":U
            WIDTH 7 COLUMN-FGCOLOR 5 LABEL-FGCOLOR 5
      valberedningtemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(30)":U
            WIDTH 28.5
      valberedningtemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
            COLUMN-FGCOLOR 2 LABEL-FGCOLOR 2
      valberedningtemp.DELNR COLUMN-LABEL "Del!nr" FORMAT ">99":U
            COLUMN-FGCOLOR 2 LABEL-FGCOLOR 2
      valberedningtemp.AKTIV COLUMN-LABEL "Aktiv/!Inaktiv" FORMAT "Aktiv/Inaktiv":U
      valberedningtemp.ANVANDARE COLUMN-LABEL "Utfärdare" FORMAT "x(12)":U
  ENABLE
      valberedningtemp.OMRADE
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS MULTIPLE SIZE 33.25 BY 12.75
         TITLE "Arbeta vidare med".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BTN_UPP AT ROW 3 COL 106
     TOG_AKT AT ROW 3.92 COL 21.75
     TOG_INAKT AT ROW 3.92 COL 33.38
     BTN_KALK AT ROW 5.13 COL 106
     CMB_OMR AT ROW 5.5 COL 19.5 COLON-ALIGNED
     CMB_UTF AT ROW 7 COL 19.5 COLON-ALIGNED
     BTN_LIST AT ROW 7.25 COL 106
     BTN_HAMT AT ROW 8.75 COL 21.5
     BTN_INK AT ROW 9.38 COL 106
     BRW_URBER AT ROW 10 COL 2
     BRW_VBER AT ROW 10 COL 70.5
     BTN_ATG AT ROW 11.5 COL 106
     BTN_ALLOVER AT ROW 13 COL 64
     BTN_KOPI AT ROW 13.63 COL 106
     BTN_OVER AT ROW 15.17 COL 64
     BTN_INAKTIV AT ROW 15.75 COL 106
     BTN_BACK AT ROW 17.38 COL 64
     BTN_ADM AT ROW 17.88 COL 106
     BTN_ALLBACK AT ROW 19.58 COL 64
     BTN_LAS AT ROW 20 COL 106
     BTN_EXP AT ROW 22.13 COL 106
     BTN_NY AT ROW 23.25 COL 75
     BTN_BORT AT ROW 23.25 COL 89.5
     BTN_IMP AT ROW 24.25 COL 106.13
     FILL-IN_AONR AT ROW 25 COL 16.5 COLON-ALIGNED
     FILL-IN-BERNR AT ROW 26.25 COL 16.5 COLON-ALIGNED
     FILL-IN-BEN AT ROW 26.25 COL 42 COLON-ALIGNED
     FILL-IN-HBERNR AT ROW 26.25 COL 89.5 COLON-ALIGNED AUTO-RETURN 
     BTN_AVB AT ROW 26.25 COL 106
     FILL-IN-AOTEXT AT ROW 1.46 COL 2.38 NO-LABEL
     FILL-IN-SOKTEXT AT ROW 23.5 COL 3 NO-LABEL
     FILL-IN-HTEXT AT ROW 25.25 COL 70 COLON-ALIGNED NO-LABEL
     RECT-20 AT ROW 25 COL 70.63
     RECT-22 AT ROW 23.25 COL 2
     RECT-5 AT ROW 1.25 COL 105
     RECT-53 AT ROW 1.25 COL 1
     "Funktioner:" VIEW-AS TEXT
          SIZE 14.5 BY .92 AT ROW 1.75 COL 107.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 124.38 BY 27.46.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: urberedningtemp T "?" NO-UNDO temp-db urberedningtemp
      TABLE: valberedningtemp T "?" NO-UNDO temp-db valberedningtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Beredning"
         HEIGHT             = 27.58
         WIDTH              = 124.5
         MAX-HEIGHT         = 30.04
         MAX-WIDTH          = 128
         VIRTUAL-HEIGHT     = 30.04
         VIRTUAL-WIDTH      = 128
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WINDOW-1
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
/* BROWSE-TAB BRW_URBER BTN_INK FRAME-A */
/* BROWSE-TAB BRW_VBER BRW_URBER FRAME-A */
ASSIGN 
       BRW_VBER:HIDDEN  IN FRAME FRAME-A                = TRUE
       BRW_VBER:MAX-DATA-GUESS IN FRAME FRAME-A         = 1000.

/* SETTINGS FOR BUTTON BTN_ADM IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BTN_BORT IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BTN_EXP IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BTN_IMP IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BTN_INAKTIV IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BTN_INK IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BTN_KALK IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BTN_LAS IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BTN_LIST IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BTN_NY IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BTN_UPP IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-AOTEXT IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FILL-IN-HTEXT IN FRAME FRAME-A
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-SOKTEXT IN FRAME FRAME-A
   NO-ENABLE ALIGN-L                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_URBER
/* Query rebuild information for BROWSE BRW_URBER
     _TblList          = "Temp-Tables.urberedningtemp"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.urberedningtemp.OMRADE
"urberedningtemp.OMRADE" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.urberedningtemp.BERNR
"urberedningtemp.BERNR" "Ber.nr" ? "integer" ? 5 ? ? 5 ? no ? no no "7" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.urberedningtemp.BENAMNING
"urberedningtemp.BENAMNING" "Benämning" ? "character" ? ? ? ? ? ? no ? no no "26" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.urberedningtemp.AONR
"urberedningtemp.AONR" "Aonr" ? "character" ? 2 ? ? 2 ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.urberedningtemp.DELNR
"urberedningtemp.DELNR" "Del!nr" ">99" "integer" ? 2 ? ? 2 ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.urberedningtemp.AKTIV
"urberedningtemp.AKTIV" "Aktiv/!Inaktiv" "Aktiv/Inaktiv" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.urberedningtemp.ANVANDARE
"urberedningtemp.ANVANDARE" "Utfärdare" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_URBER */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VBER
/* Query rebuild information for BROWSE BRW_VBER
     _TblList          = "Temp-Tables.valberedningtemp"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.valberedningtemp.OMRADE
"valberedningtemp.OMRADE" ? ? "character" ? ? ? ? ? ? yes ? no no "6.5" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.valberedningtemp.BERNR
"valberedningtemp.BERNR" "Ber.nr" ? "integer" ? 5 ? ? 5 ? no ? no no "7" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.valberedningtemp.BENAMNING
"valberedningtemp.BENAMNING" "Benämning" ? "character" ? ? ? ? ? ? no ? no no "28.5" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.valberedningtemp.AONR
"valberedningtemp.AONR" "Aonr" ? "character" ? 2 ? ? 2 ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.valberedningtemp.DELNR
"valberedningtemp.DELNR" "Del!nr" ">99" "integer" ? 2 ? ? 2 ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.valberedningtemp.AKTIV
"valberedningtemp.AKTIV" "Aktiv/!Inaktiv" "Aktiv/Inaktiv" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.valberedningtemp.ANVANDARE
"valberedningtemp.ANVANDARE" "Utfärdare" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VBER */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_VBER
&Scoped-define SELF-NAME BRW_VBER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VBER WINDOW-1
ON MOUSE-SELECT-CLICK OF BRW_VBER IN FRAME FRAME-A /* Arbeta vidare med */
DO:
   MESSAGE "3" VIEW-AS ALERT-BOX.
   APPLY "value-changed" TO brw_vber.
   MESSAGE "4" VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VBER WINDOW-1
ON VALUE-CHANGED OF BRW_VBER IN FRAME FRAME-A /* Arbeta vidare med */
DO:
   IF franvart NE 1 THEN RUN btnnykoll_UI.
   MESSAGE "2" VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ADM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ADM WINDOW-1
ON CHOOSE OF BTN_ADM IN FRAME FRAME-A /* Administration */
DO:
   
   
   IF Guru.Konstanter:globanv = "CELPAO" OR Guru.Konstanter:globanv = "CLHA" AND Guru.Konstanter:globforetag = "VAST" THEN DO:
      RUN BERADMU.W.
   END.
   ELSE RUN BERADM.W.
   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ATG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ATG WINDOW-1
ON CHOOSE OF BTN_ATG IN FRAME FRAME-A /* Åtgärder */
DO:   
   RUN btnatg_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avsluta */
DO:
   IF franvart = 1 THEN DO:
      MESSAGE "Vill du spara dina valda beredningar för snabbare åtkomst ?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val AS LOGICAL.
      IF val = TRUE THEN DO:
         tthandle = TEMP-TABLE valberedningtemp:HANDLE.
         FIND FIRST sparaladdatemp NO-ERROR.
         IF NOT AVAILABLE sparaladdatemp THEN CREATE sparaladdatemp.
         ASSIGN
         sparaladdatemp.GLOBANV = Guru.Konstanter:globanv /*Användare, i detta fall ELPAO*/
         sparaladdatemp.BENAMNING = "BER" /*Benämnings sufix, i detta fall ELPAO$STOR*/
         sparaladdatemp.TABVAL = "BEREDNING" /*Tabellnamn*/
         sparaladdatemp.FALTVALAO = "OMRADE" /*CHARACTER field*/
         sparaladdatemp.FALTVALDEL = "BERNR" /*Integer field*/
         sparaladdatemp.FALTVALDATE = "".  /*DATE field*/
         
      END.
   END.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-1
ON CHOOSE OF BTN_BORT IN FRAME FRAME-A /* Ta bort */
DO:
   RUN bort_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_EXP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EXP WINDOW-1
ON CHOOSE OF BTN_EXP IN FRAME FRAME-A /* Export */
DO:
   RUN expimp_UI (INPUT 1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_HAMT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_HAMT WINDOW-1
ON CHOOSE OF BTN_HAMT IN FRAME FRAME-A /* Hämta och visa urval */
DO:   
   RUN hamta_UI (INPUT 1).
/*    RUN storapp_UI. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_IMP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_IMP WINDOW-1
ON CHOOSE OF BTN_IMP IN FRAME FRAME-A /* Import */
DO:
   RUN expimp_UI (INPUT 2).   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_INAKTIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_INAKTIV WINDOW-1
ON CHOOSE OF BTN_INAKTIV IN FRAME FRAME-A /* Aktiv/inaktiv */
DO:
   RUN inaktiv_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_INK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_INK WINDOW-1
ON CHOOSE OF BTN_INK IN FRAME FRAME-A /* Inköp */
DO:
   RUN inkop_UI.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KALK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KALK WINDOW-1
ON CHOOSE OF BTN_KALK IN FRAME FRAME-A /* Bereda */
DO: 
   RUN bereda_UI.         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KOPI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KOPI WINDOW-1
ON CHOOSE OF BTN_KOPI IN FRAME FRAME-A /* Kopiera */
DO:   
   RUN btnkopi_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_LAS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_LAS WINDOW-1
ON CHOOSE OF BTN_LAS IN FRAME FRAME-A /* Låsta beredningar */
DO:
   
   
   RUN BERLAS.W.
   
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_LIST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_LIST WINDOW-1
ON CHOOSE OF BTN_LIST IN FRAME FRAME-A /* Listor */
DO:
   RUN listor_UI.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WINDOW-1
ON CHOOSE OF BTN_NY IN FRAME FRAME-A /* Ny */
DO:
   RUN ny_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPP WINDOW-1
ON CHOOSE OF BTN_UPP IN FRAME FRAME-A /* Beredningshuvud */
DO:
   RUN andra_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_OMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_OMR WINDOW-1
ON VALUE-CHANGED OF CMB_OMR IN FRAME FRAME-A /* Område */
DO:
   CMB_OMR = INPUT CMB_OMR.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-HBERNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-HBERNR WINDOW-1
ON ANY-KEY OF FILL-IN-HBERNR IN FRAME FRAME-A /* Beredningsnr */
DO:
   IF KEYFUNCTION(LASTKEY) = ("CURSOR-RIGHT") OR KEYFUNCTION(LASTKEY) = ("CURSOR-LEFT") OR
      KEYFUNCTION(LASTKEY) = ("CURSOR-DOWN") OR KEYFUNCTION(LASTKEY) = ("CURSOR-UP") THEN DO :
      APPLY KEYFUNCTION(LASTKEY).
      RETURN NO-APPLY.
   END.

   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-HBERNR IN FRAME {&FRAME-NAME}.
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-HBERNR WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-HBERNR IN FRAME FRAME-A /* Beredningsnr */
DO:
   RUN hamta_UI (INPUT 2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_AKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_AKT WINDOW-1
ON VALUE-CHANGED OF TOG_AKT IN FRAME FRAME-A /* Aktiva */
DO:
   TOG_AKT = INPUT TOG_AKT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_INAKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_INAKT WINDOW-1
ON VALUE-CHANGED OF TOG_INAKT IN FRAME FRAME-A /* Inaktiva */
DO:
   TOG_INAKT = INPUT TOG_INAKT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_URBER
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   
   RUN disable_UI.
END.
   

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {&WINDOW-NAME}:HIDDEN = FALSE.
   {&WINDOW-NAME}:MOVE-TO-TOP ().  
   RUN allstartbrw_UI.
   RUN huvud_UI.
   RUN enable_UI.
   RUN bersek_UI.
   RUN frankontroll_UI.
   ENABLE BTN_LIST WITH FRAME {&FRAME-NAME}.
   
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   ASSIGN
   urberedningtemp.OMRADE:READ-ONLY IN BROWSE BRW_URBER = TRUE    
   valberedningtemp.OMRADE:READ-ONLY IN BROWSE  BRW_VBER = TRUE.
   RUN xDYNBRW.P PERSISTENT SET brwproc[{&LEFT-BROWSE}] 
      (INPUT BRW_URBER:HANDLE IN FRAME {&FRAME-NAME}).
   RUN xDYNBRW.P PERSISTENT SET brwproc[{&RIGHT-BROWSE}] 
      (INPUT BRW_VBER:HANDLE IN FRAME {&FRAME-NAME}).
   CREATE valberedningtemp.
   ASSIGN
   valberedningtemp.BENAMNING = "click here"
   valberedningtemp.omrade = "1".
   CREATE valberedningtemp.
   ASSIGN
   valberedningtemp.BENAMNING = "click here"
   valberedningtemp.omrade = "2".
   RUN openbdyn_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").  
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra_UI WINDOW-1 
PROCEDURE andra_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE anvbort_UI WINDOW-1 
PROCEDURE anvbort_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE anvkoll_UI WINDOW-1 
PROCEDURE anvkoll_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   DEFINE INPUT PARAMETER visamedd AS LOGICAL NO-UNDO.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE beranv_UI WINDOW-1 
PROCEDURE beranv_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bereda_UI WINDOW-1 
PROCEDURE bereda_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bere_UI WINDOW-1 
PROCEDURE bere_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE berkoll_UI WINDOW-1 
PROCEDURE berkoll_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bersek_UI WINDOW-1 
PROCEDURE bersek_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   
           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bort_UI WINDOW-1 
PROCEDURE bort_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnatg_UI WINDOW-1 
PROCEDURE btnatg_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnkopi_UI WINDOW-1 
PROCEDURE btnkopi_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/     
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE btnnykoll_UI WINDOW-1 
PROCEDURE btnnykoll_UI :
/*-----------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-1  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
  THEN DELETE WIDGET WINDOW-1.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-1  _DEFAULT-ENABLE
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
  DISPLAY TOG_AKT TOG_INAKT CMB_OMR CMB_UTF FILL-IN_AONR FILL-IN-BERNR 
          FILL-IN-BEN FILL-IN-HBERNR FILL-IN-AOTEXT FILL-IN-SOKTEXT 
          FILL-IN-HTEXT 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE TOG_AKT TOG_INAKT CMB_OMR CMB_UTF BTN_HAMT BRW_URBER BRW_VBER BTN_ATG 
         BTN_ALLOVER BTN_KOPI BTN_OVER BTN_BACK BTN_ALLBACK FILL-IN_AONR 
         FILL-IN-BERNR FILL-IN-BEN FILL-IN-HBERNR BTN_AVB RECT-20 RECT-22 
         RECT-5 RECT-53 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE expimp_UI WINDOW-1 
PROCEDURE expimp_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER val AS INTEGER NO-UNDO.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE frankontroll_UI WINDOW-1 
PROCEDURE frankontroll_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hamta_UI WINDOW-1 
PROCEDURE hamta_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER hamtval AS INTEGER NO-UNDO.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE huvud_UI WINDOW-1 
PROCEDURE huvud_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   IF franvart = 1 THEN DO:
      status-ok = CMB_UTF:ADD-FIRST("Alla") IN FRAME {&FRAME-NAME}.
      status-ok = CMB_UTF:ADD-LAST(globanv).
      ASSIGN CMB_UTF:SCREEN-VALUE = "Alla".
      status-ok = CMB_OMR:ADD-FIRST("Alla").   
      
      FOR EACH omrtemp:
         status-ok = CMB_OMR:ADD-LAST(omrtemp.NAMN).
      END.
      vartpro = "".
      IF Guru.Konstanter:globomr = "" OR Guru.Konstanter:globallao = TRUE THEN DO:
         ASSIGN CMB_OMR:SCREEN-VALUE = "Alla".
         CMB_OMR = INPUT CMB_OMR.
         DISPLAY CMB_OMR WITH FRAME {&FRAME-NAME}.
      END.                  
   END.                                                                    
   
   
   
   
   
   vart = "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE inaktiv_UI WINDOW-1 
PROCEDURE inaktiv_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE inkop_UI WINDOW-1 
PROCEDURE inkop_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE listor_UI WINDOW-1 
PROCEDURE listor_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ny_UI WINDOW-1 
PROCEDURE ny_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val_UI WINDOW-1 
PROCEDURE val_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION brwval WINDOW-1 
FUNCTION brwval RETURNS LOGICAL
  (  ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   RETURN FALSE.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION felkoll WINDOW-1 
FUNCTION felkoll RETURNS LOGICAL
  ( INPUT felmed AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
   RETURN FALSE.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

