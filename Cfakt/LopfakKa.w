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
{FAKTPLANTEMP.I}
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR efaktkredtemp.
/* Local Variable Definitions ---                                       */
&Scoped-define NEW NEW SHARED 
{FAKKOTEMP.I}
&Scoped-define NEW
{ALLDEF.I}
{GLOBVAR2DEL1.I}
{EXTRATAB.I}
&Scoped-define NEW 
{FAKTTYPDEF.I}

&Scoped-define NEW NEW                          
{FAKTTEMP.I}
{HOPALLA.I}
{EXTRADATA.I}
{FAKTSTART.I}
{ORGPRIS.I}
{FAKTBILAG.I}
DEFINE NEW SHARED VARIABLE vartyp AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE faktupphmth AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE fakthmth AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE skarpfaktok AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.   
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE fbestapph AS HANDLE NO-UNDO.

DEFINE VARIABLE infakplannr AS INTEGER NO-UNDO.
DEFINE VARIABLE kreditnrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE feltextvar AS CHARACTER FORMAT "X(10)" NO-UNDO.
DEFINE VARIABLE fakkoproch AS HANDLE NO-UNDO. 
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
DEFINE VARIABLE sumanstf AS CHARACTER NO-UNDO.
DEFINE VARIABLE sumanstraf AS CHARACTER NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO. 
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.  
DEFINE VARIABLE arkivvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE gamanv AS CHARACTER NO-UNDO.

DEFINE VARIABLE resber AS DECIMAL NO-UNDO.

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


DEFINE VARIABLE skarpvar AS INTEGER NO-UNDO.
DEFINE VARIABLE vararbkost AS DECIMAL NO-UNDO.   
DEFINE VARIABLE vartrakost AS DECIMAL NO-UNDO.
DEFINE VARIABLE sluttotpris AS DECIMAL NO-UNDO.
DEFINE BUFFER gfaktempbuff FOR gfaktemp.
DEFINE BUFFER sumtidtempbuff FOR sumtidtemp.
DEFINE BUFFER faktupparbbuff FOR faktupparbtemp.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-C
&Scoped-define BROWSE-NAME BRW_ALLT

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES sumpers faktfriatemp gfaktemp momstemp ~
kosttemp faktstarttemp sumtidtemp faktupparbtemp

/* Definitions for BROWSE BRW_ALLT                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_ALLT sumpers.VIBEFATTNING sumpers.AONR ~
sumpers.DELNR sumpers.TIMMAR sumpers.OTIMMAR sumpers.BELOPP 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ALLT 
&Scoped-define QUERY-STRING-BRW_ALLT FOR EACH sumpers NO-LOCK ~
    BY sumpers.AONR ~
       BY sumpers.DELNR ~
        BY sumpers.VIBEFATTNING
&Scoped-define OPEN-QUERY-BRW_ALLT OPEN QUERY BRW_ALLT FOR EACH sumpers NO-LOCK ~
    BY sumpers.AONR ~
       BY sumpers.DELNR ~
        BY sumpers.VIBEFATTNING.
&Scoped-define TABLES-IN-QUERY-BRW_ALLT sumpers
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ALLT sumpers


/* Definitions for BROWSE BRW_FRI                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_FRI faktfriatemp.FAKTTEXT ~
faktfriatemp.ENHET faktfriatemp.ANTAL faktfriatemp.PRIS_ENHET ~
faktfriatemp.TOTKALK faktfriatemp.TOTALT faktfriatemp.FAKTURERAD ~
faktfriatemp.AONR faktfriatemp.DELNR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_FRI 
&Scoped-define QUERY-STRING-BRW_FRI FOR EACH faktfriatemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_FRI OPEN QUERY BRW_FRI FOR EACH faktfriatemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_FRI faktfriatemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_FRI faktfriatemp


/* Definitions for BROWSE BRW_GKOST                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_GKOST gfaktemp.TYPTEXT gfaktemp.TOTALT ~
gfaktemp.MTRL gfaktemp.OVRIG gfaktemp.ARBKOST gfaktemp.OBELOPP ~
gfaktemp.TRAKT gfaktemp.LONTILL gfaktemp.RES gfaktemp.KBELOPP 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_GKOST 
&Scoped-define QUERY-STRING-BRW_GKOST FOR EACH gfaktemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_GKOST OPEN QUERY BRW_GKOST FOR EACH gfaktemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_GKOST gfaktemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_GKOST gfaktemp


/* Definitions for BROWSE BRW_K4                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_K4 momstemp.MOMSKOD momstemp.MOMSTEXT ~
momstemp.MOMSEXTERNT momstemp.MOMSNR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K4 
&Scoped-define QUERY-STRING-BRW_K4 FOR EACH momstemp NO-LOCK ~
    BY momstemp.MOMSNR ~
       BY momstemp.MOMSKOD
&Scoped-define OPEN-QUERY-BRW_K4 OPEN QUERY BRW_K4 FOR EACH momstemp NO-LOCK ~
    BY momstemp.MOMSNR ~
       BY momstemp.MOMSKOD.
&Scoped-define TABLES-IN-QUERY-BRW_K4 momstemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K4 momstemp


/* Definitions for BROWSE BRW_KOST                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_KOST kosttemp.FAKTNR kosttemp.BENAMNING ~
kosttemp.AONR kosttemp.DELNR kosttemp.BOKKONTO kosttemp.TRAKTKOST ~
kosttemp.PERSKOST kosttemp.MASKKOST kosttemp.FRTJPAKR kosttemp.MTRL ~
kosttemp.MTRLPAKR kosttemp.OVRKR kosttemp.MED 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KOST kosttemp.FAKTNR ~
kosttemp.BENAMNING kosttemp.BOKKONTO kosttemp.TRAKTKOST kosttemp.PERSKOST ~
kosttemp.MASKKOST kosttemp.FRTJPAKR kosttemp.MTRL kosttemp.MTRLPAKR ~
kosttemp.OVRKR kosttemp.MED 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_KOST kosttemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_KOST kosttemp
&Scoped-define QUERY-STRING-BRW_KOST FOR EACH kosttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KOST OPEN QUERY BRW_KOST FOR EACH kosttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KOST kosttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KOST kosttemp


/* Definitions for BROWSE BRW_KOSTMOMS                                  */
&Scoped-define FIELDS-IN-QUERY-BRW_KOSTMOMS kosttemp.FAKTNR ~
kosttemp.BENAMNING kosttemp.AONR kosttemp.DELNR kosttemp.MOMSKOD ~
kosttemp.MOMSEXTERNT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KOSTMOMS 
&Scoped-define QUERY-STRING-BRW_KOSTMOMS FOR EACH kosttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KOSTMOMS OPEN QUERY BRW_KOSTMOMS FOR EACH kosttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KOSTMOMS kosttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KOSTMOMS kosttemp


/* Definitions for BROWSE BRW_PLAN                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_PLAN faktstarttemp.PLAN% ~
faktstarttemp.PLANDATUM faktstarttemp.FRITEXT faktstarttemp.BELOPP ~
faktstarttemp.FAKTURADATUM faktstarttemp.FAKTURERAD faktstarttemp.VFAKTNR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PLAN faktstarttemp.FRITEXT ~
faktstarttemp.BELOPP faktstarttemp.FAKTURERAD 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_PLAN faktstarttemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_PLAN faktstarttemp
&Scoped-define QUERY-STRING-BRW_PLAN FOR EACH faktstarttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_PLAN OPEN QUERY BRW_PLAN FOR EACH faktstarttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_PLAN faktstarttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PLAN faktstarttemp


/* Definitions for BROWSE BRW_TID                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_TID sumtidtemp.PERSBEF sumtidtemp.DATUM ~
sumtidtemp.START sumtidtemp.SLUT sumtidtemp.TIMMAR sumtidtemp.BELOPP ~
sumtidtemp.OTIMMAR sumtidtemp.OBELOPP sumtidtemp.VITRAKT ~
sumtidtemp.TRAKTANTAL sumtidtemp.TBELOPP sumtidtemp.VILART ~
sumtidtemp.LONTILLANTAL sumtidtemp.LONKOST sumtidtemp.RESTIM ~
sumtidtemp.RESKOSTDEC sumtidtemp.AONR sumtidtemp.DELNR sumtidtemp.MED 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TID sumtidtemp.START ~
sumtidtemp.SLUT sumtidtemp.TIMMAR sumtidtemp.OTIMMAR sumtidtemp.RESTIM ~
sumtidtemp.MED 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_TID sumtidtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_TID sumtidtemp
&Scoped-define QUERY-STRING-BRW_TID FOR EACH sumtidtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_TID OPEN QUERY BRW_TID FOR EACH sumtidtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_TID sumtidtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TID sumtidtemp


/* Definitions for BROWSE BRW_UPP                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_UPP faktupparbtemp.AONR ~
faktupparbtemp.DELNR faktupparbtemp.UPLAN% faktupparbtemp.UPPARB% ~
faktupparbtemp.UPPBELOPP faktupparbtemp.FAKT% faktupparbtemp.FAKTBELOPP ~
faktupparbtemp.FRITEXT faktupparbtemp.KRITERIUM faktupparbtemp.FAKTURERAD ~
faktupparbtemp.FAKTURADATUM 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UPP faktupparbtemp.FRITEXT ~
faktupparbtemp.FAKTURERAD 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_UPP faktupparbtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_UPP faktupparbtemp
&Scoped-define QUERY-STRING-BRW_UPP FOR EACH faktupparbtemp NO-LOCK ~
    BY faktupparbtemp.UPPARB%
&Scoped-define OPEN-QUERY-BRW_UPP OPEN QUERY BRW_UPP FOR EACH faktupparbtemp NO-LOCK ~
    BY faktupparbtemp.UPPARB%.
&Scoped-define TABLES-IN-QUERY-BRW_UPP faktupparbtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UPP faktupparbtemp


/* Definitions for FRAME FRAME-C                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-C ~
    ~{&OPEN-QUERY-BRW_GKOST}~
    ~{&OPEN-QUERY-BRW_K4}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS TOG_DIFF FBTN_OK FBTN_VISFAK FBTN_MOMSBORT ~
FBTN_MOMS FBTN_MED BTN_SNABB BTN_OK BTN_FRINY BTN_FRIAND BTN_FRIBORT ~
BTN_AVB 
&Scoped-Define DISPLAYED-FIELDS vfaktplantemp.FAKTNR vfaktplantemp.NAMN 
&Scoped-define DISPLAYED-TABLES vfaktplantemp
&Scoped-define FIRST-DISPLAYED-TABLE vfaktplantemp
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_FDELNR TOG_DIFF TOG_MOMS 

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

DEFINE MENU POPUP-MENU-BRW_TID 
       MENU-ITEM m_Ta_med_markerade_posterTID LABEL "Ta med markerade poster"
       MENU-ITEM m_Ta_ej_med_markerade_posterTID LABEL "Ta ej med markerade poster".


/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 5.5 BY 1.75.

DEFINE BUTTON BTN_FRIAND 
     LABEL "Ändra fripost":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_FRIBORT 
     LABEL "Ta bort fripost":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_FRINY 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 5.5 BY 1.75.

DEFINE BUTTON BTN_SNABB 
     LABEL "Snabbspara" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON FBTN_MED 
     LABEL "Text till faktura" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_MOMS 
     LABEL "Kontering" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_MOMSBORT 
     LABEL "Ta bort kontering" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_OK 
     LABEL "Fakturera" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VISFAK 
     LABEL "Visa faktura":L 
     SIZE 14 BY 1.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-TOMDAT AS DATE FORMAT "99/99/99":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_FDELNR AS INTEGER FORMAT "9999999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE RAD_VISA AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Tidskrivning", 1,
"Kostnadsreg.", 2,
"Fri komplettering", 3
     SIZE 96.88 BY .83 NO-UNDO.

DEFINE VARIABLE TOG_DIFF AS LOGICAL INITIAL no 
     LABEL "Differentierad moms" 
     VIEW-AS TOGGLE-BOX
     SIZE 23.75 BY .67 NO-UNDO.

DEFINE VARIABLE TOG_MOMS AS LOGICAL INITIAL no 
     LABEL "Kontoberäkningar utförda." 
     VIEW-AS TOGGLE-BOX
     SIZE 29.75 BY .67 TOOLTIP "Om kontoberäkningar är utförda kan inga ändringar göras!" NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_ALLT FOR 
      sumpers SCROLLING.

DEFINE QUERY BRW_FRI FOR 
      faktfriatemp SCROLLING.

DEFINE QUERY BRW_GKOST FOR 
      gfaktemp SCROLLING.

DEFINE QUERY BRW_K4 FOR 
      momstemp SCROLLING.

DEFINE QUERY BRW_KOST FOR 
      kosttemp SCROLLING.

DEFINE QUERY BRW_KOSTMOMS FOR 
      kosttemp SCROLLING.

DEFINE QUERY BRW_PLAN FOR 
      faktstarttemp SCROLLING.

DEFINE QUERY BRW_TID FOR 
      sumtidtemp SCROLLING.

DEFINE QUERY BRW_UPP FOR 
      faktupparbtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_ALLT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ALLT WINDOW-3 _STRUCTURED
  QUERY BRW_ALLT DISPLAY
      sumpers.VIBEFATTNING COLUMN-LABEL "Rubrik" FORMAT "x(30)":U
      sumpers.AONR COLUMN-LABEL "Aonr" FORMAT "X(7)":U
      sumpers.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      sumpers.TIMMAR COLUMN-LABEL "Antal" FORMAT "->>>>>>>9.99":U
      sumpers.OTIMMAR COLUMN-LABEL "A´pris" FORMAT "->>>>9":U
      sumpers.BELOPP COLUMN-LABEL "Summa" FORMAT "->>>>>>>>>>9":U
            COLUMN-FGCOLOR 12 LABEL-FGCOLOR 12
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 79.5 BY 18.79.

DEFINE BROWSE BRW_FRI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_FRI WINDOW-3 _STRUCTURED
  QUERY BRW_FRI NO-LOCK DISPLAY
      faktfriatemp.FAKTTEXT COLUMN-LABEL "Fakt.text" FORMAT "X(256)":U
            WIDTH 50
      faktfriatemp.ENHET COLUMN-LABEL "Sort" FORMAT "x(2)":U
      faktfriatemp.ANTAL COLUMN-LABEL "Antal" FORMAT "->>>>9":U
      faktfriatemp.PRIS_ENHET COLUMN-LABEL "Pris/!enhet" FORMAT "->>>>>>>9":U
      faktfriatemp.TOTKALK COLUMN-LABEL "Tot.kalk" FORMAT "->>>>>>>9":U
      faktfriatemp.TOTALT COLUMN-LABEL "Offert" FORMAT "->>>>>>>9":U
      faktfriatemp.FAKTURERAD COLUMN-LABEL "Ta!med" FORMAT "Ja/Nej":U
      faktfriatemp.AONR FORMAT "X(6)":U
      faktfriatemp.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 108 BY 17
         TITLE "Fri komplettering av faktura".

DEFINE BROWSE BRW_GKOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_GKOST WINDOW-3 _STRUCTURED
  QUERY BRW_GKOST DISPLAY
      gfaktemp.TYPTEXT FORMAT "X(20)":U
      gfaktemp.TOTALT FORMAT "->>>>>>>9":U COLUMN-FGCOLOR 12 LABEL-FGCOLOR 12
      gfaktemp.MTRL COLUMN-LABEL "Materiel!kostnad" FORMAT "->>>>>>>9":U
      gfaktemp.OVRIG COLUMN-LABEL "Övrig!kostnad" FORMAT "->>>>>>9":U
      gfaktemp.ARBKOST COLUMN-LABEL "Arbets!kostnad" FORMAT "->>>>>>>9":U
      gfaktemp.OBELOPP COLUMN-LABEL "Övertid!kostnad" FORMAT "->>>>>>9":U
      gfaktemp.TRAKT COLUMN-LABEL "Trakt.!kostnad" FORMAT "->>>>>9":U
      gfaktemp.LONTILL COLUMN-LABEL "Lönetill.!kostnad" FORMAT "->>>>>>9":U
      gfaktemp.RES COLUMN-LABEL "Res!kostnad" FORMAT "->>>>>>9":U
      gfaktemp.KBELOPP COLUMN-LABEL "Fr.tjänst!kost." FORMAT "->>>>>>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 104.38 BY 4.38.

DEFINE BROWSE BRW_K4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K4 WINDOW-3 _STRUCTURED
  QUERY BRW_K4 NO-LOCK DISPLAY
      momstemp.MOMSKOD COLUMN-LABEL "Moms!konto" FORMAT "X(5)":U
            WIDTH 6.5
      momstemp.MOMSTEXT COLUMN-LABEL "Text" FORMAT "X(40)":U WIDTH 22.5
      momstemp.MOMSEXTERNT COLUMN-LABEL "Moms!i %" FORMAT ">>9.99":U
      momstemp.MOMSNR COLUMN-LABEL "Moms!kod" FORMAT "X(4)":U WIDTH 5
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 44 BY 18.79
         TITLE "Momstabell".

DEFINE BROWSE BRW_KOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KOST WINDOW-3 _STRUCTURED
  QUERY BRW_KOST NO-LOCK DISPLAY
      kosttemp.FAKTNR COLUMN-LABEL "Ver-nr" FORMAT "x(15)":U WIDTH 13
      kosttemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
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
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 109 BY 18.79
         TITLE "Kostnadsregistrering - ~"Klicka~" på de som ej ska faktureras i MED-fältet".

DEFINE BROWSE BRW_KOSTMOMS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KOSTMOMS WINDOW-3 _STRUCTURED
  QUERY BRW_KOSTMOMS NO-LOCK DISPLAY
      kosttemp.FAKTNR COLUMN-LABEL "Ver-nr" FORMAT "x(15)":U
      kosttemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(22)":U
            WIDTH 17
      kosttemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      kosttemp.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      kosttemp.MOMSKOD COLUMN-LABEL "Moms!konto" FORMAT "X(5)":U
      kosttemp.MOMSEXTERNT COLUMN-LABEL "Moms!i %" FORMAT ">>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 59.5 BY 18.79
         TITLE "Kostnadsregistrering - ~"Markera de som skall ha annan momssats~"".

DEFINE BROWSE BRW_PLAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PLAN WINDOW-3 _STRUCTURED
  QUERY BRW_PLAN NO-LOCK DISPLAY
      faktstarttemp.PLAN% COLUMN-LABEL "Andel! i %" FORMAT ">>9":U
      faktstarttemp.PLANDATUM COLUMN-LABEL "Planerat!datum" FORMAT "99/99/99":U
      faktstarttemp.FRITEXT COLUMN-LABEL "Text" FORMAT "X(30)":U
      faktstarttemp.BELOPP COLUMN-LABEL "Belopp" FORMAT "->>>>>>9.99":U
      faktstarttemp.FAKTURADATUM COLUMN-LABEL "Fakturerat!datum" FORMAT "99/99/99":U
      faktstarttemp.FAKTURERAD COLUMN-LABEL "Fakturera" FORMAT "Ja/Nej":U
      faktstarttemp.VFAKTNR COLUMN-LABEL "Fakturanr" FORMAT ">>>>>>>>9":U
  ENABLE
      faktstarttemp.FRITEXT
      faktstarttemp.BELOPP
      faktstarttemp.FAKTURERAD
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 92.25 BY 18.79
         TITLE "Faktureringstidpunkter".

DEFINE BROWSE BRW_TID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TID WINDOW-3 _STRUCTURED
  QUERY BRW_TID NO-LOCK DISPLAY
      sumtidtemp.PERSBEF COLUMN-LABEL "Enhet/Sign!Befattning" FORMAT "X(23)":U
            WIDTH 12
      sumtidtemp.DATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
      sumtidtemp.START COLUMN-LABEL "Start!tid" FORMAT ">9.99":U
            COLUMN-FGCOLOR 1 LABEL-FGCOLOR 1
      sumtidtemp.SLUT COLUMN-LABEL "Slut!tid" FORMAT ">9.99":U
            COLUMN-FGCOLOR 3 LABEL-FGCOLOR 3
      sumtidtemp.TIMMAR COLUMN-LABEL "Timmar" FORMAT "->9.9<":U
            COLUMN-FGCOLOR 4 LABEL-FGCOLOR 4
      sumtidtemp.BELOPP COLUMN-LABEL "Arbets!kost." FORMAT "->>>>>9":U
      sumtidtemp.OTIMMAR COLUMN-LABEL "Över.!timmar" FORMAT "->9.9<":U
      sumtidtemp.OBELOPP COLUMN-LABEL "Övertid!kost." FORMAT "->>>>>9":U
      sumtidtemp.VITRAKT COLUMN-LABEL "Lart" FORMAT "X(4)":U
      sumtidtemp.TRAKTANTAL COLUMN-LABEL "Antal" FORMAT "->9.9":U
      sumtidtemp.TBELOPP COLUMN-LABEL "Trakt!kost." FORMAT "->>>9":U
      sumtidtemp.VILART COLUMN-LABEL "Lart" FORMAT "X(7)":U WIDTH 4
      sumtidtemp.LONTILLANTAL COLUMN-LABEL "Antal" FORMAT ">>>9.9<":U
      sumtidtemp.LONKOST COLUMN-LABEL "Löntill!kost." FORMAT "->>>>>9":U
      sumtidtemp.RESTIM COLUMN-LABEL "Res.!tim." FORMAT "->9.9<":U
      sumtidtemp.RESKOSTDEC COLUMN-LABEL "Res.!kost." FORMAT "->>>>9":U
      sumtidtemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      sumtidtemp.DELNR COLUMN-LABEL "Del!nr" FORMAT ">99":U
      sumtidtemp.MED COLUMN-LABEL "Ta!med" FORMAT "Ja/Nej":U
  ENABLE
      sumtidtemp.START
      sumtidtemp.SLUT
      sumtidtemp.TIMMAR
      sumtidtemp.OTIMMAR
      sumtidtemp.RESTIM
      sumtidtemp.MED
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 109 BY 18.79
         TITLE "Tidskrivning under perioden - ~"Klicka~" på de som ej ska faktureras i MED-fältet" TOOLTIP "Tryck högra mus-tangenten för att ändra befattning mm.".

DEFINE BROWSE BRW_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UPP WINDOW-3 _STRUCTURED
  QUERY BRW_UPP NO-LOCK DISPLAY
      faktupparbtemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U
      faktupparbtemp.DELNR COLUMN-LABEL "Del!nr" FORMAT "999":U
      faktupparbtemp.UPLAN% COLUMN-LABEL "Plan.!upparb.%" FORMAT ">>9":U
      faktupparbtemp.UPPARB% COLUMN-LABEL "Upparb.%" FORMAT ">>9":U
      faktupparbtemp.UPPBELOPP COLUMN-LABEL "Upparbetat!belopp" FORMAT "->>>>>>>9.99":U
      faktupparbtemp.FAKT% COLUMN-LABEL "Fakt.%" FORMAT ">>9":U
      faktupparbtemp.FAKTBELOPP COLUMN-LABEL "Fakturera!belopp" FORMAT "->>>>>>>9.99":U
      faktupparbtemp.FRITEXT FORMAT "X(30)":U
      faktupparbtemp.KRITERIUM FORMAT "X(8)":U
      faktupparbtemp.FAKTURERAD COLUMN-LABEL "Fakturerad" FORMAT "Ja/Nej":U
      faktupparbtemp.FAKTURADATUM COLUMN-LABEL "Faktura!datum" FORMAT "99/99/99":U
  ENABLE
      faktupparbtemp.FRITEXT
      faktupparbtemp.FAKTURERAD
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 109 BY 18.79
         TITLE "Upparbetade kostnader".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-C
     vfaktplantemp.FAKTNR AT ROW 1.25 COL 16 COLON-ALIGNED
          LABEL "Prel. kredit nr"
          VIEW-AS FILL-IN 
          SIZE 8 BY 1
     FILL-IN_FDELNR AT ROW 1.25 COL 25 COLON-ALIGNED NO-LABEL
     vfaktplantemp.NAMN AT ROW 1.25 COL 34.75 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 31 BY 1
     BRW_GKOST AT ROW 2.38 COL 1.5
     TOG_DIFF AT ROW 7.88 COL 1.5
     TOG_MOMS AT ROW 7.88 COL 68.13
     FBTN_OK AT ROW 8 COL 111.5
     RAD_VISA AT ROW 8.67 COL 1.5 NO-LABEL
     FBTN_VISFAK AT ROW 9.08 COL 111.5
     FBTN_MOMSBORT AT ROW 10.21 COL 111.5
     FBTN_MOMS AT ROW 10.21 COL 111.5
     BRW_K4 AT ROW 10.33 COL 1
     BRW_UPP AT ROW 10.33 COL 1.5
     BRW_TID AT ROW 10.33 COL 1.5
     BRW_ALLT AT ROW 10.33 COL 1.5
     BRW_FRI AT ROW 10.33 COL 1.5
     BRW_KOST AT ROW 10.33 COL 1.5
     BRW_PLAN AT ROW 10.33 COL 1.5
     BRW_KOSTMOMS AT ROW 10.33 COL 51
     FBTN_MED AT ROW 11.29 COL 111.5
     BTN_SNABB AT ROW 12.42 COL 111.5
     BTN_OVER AT ROW 14.17 COL 45.38
     BTN_BACK AT ROW 17.42 COL 45.38
     FILL-IN-TOMDAT AT ROW 21.83 COL 83.13 COLON-ALIGNED NO-LABEL
     BTN_OK AT ROW 27.08 COL 111.5
     BTN_FRINY AT ROW 28.17 COL 29.25
     BTN_FRIAND AT ROW 28.17 COL 51.25
     BTN_FRIBORT AT ROW 28.17 COL 73.38
     BTN_AVB AT ROW 28.17 COL 111.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125.13 BY 28.75.


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
         TITLE              = "Kredit fakturering"
         HEIGHT             = 28.42
         WIDTH              = 125
         MAX-HEIGHT         = 28.75
         MAX-WIDTH          = 125.13
         VIRTUAL-HEIGHT     = 28.75
         VIRTUAL-WIDTH      = 125.13
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
/* BROWSE-TAB BRW_GKOST NAMN FRAME-C */
/* BROWSE-TAB BRW_K4 FBTN_MOMS FRAME-C */
/* BROWSE-TAB BRW_UPP BRW_K4 FRAME-C */
/* BROWSE-TAB BRW_TID BRW_UPP FRAME-C */
/* BROWSE-TAB BRW_ALLT BRW_TID FRAME-C */
/* BROWSE-TAB BRW_FRI BRW_ALLT FRAME-C */
/* BROWSE-TAB BRW_KOST BRW_FRI FRAME-C */
/* BROWSE-TAB BRW_PLAN BRW_KOST FRAME-C */
/* BROWSE-TAB BRW_KOSTMOMS BRW_PLAN FRAME-C */
/* SETTINGS FOR BROWSE BRW_ALLT IN FRAME FRAME-C
   NO-ENABLE                                                            */
/* SETTINGS FOR BROWSE BRW_FRI IN FRAME FRAME-C
   NO-ENABLE                                                            */
ASSIGN 
       BRW_FRI:HIDDEN  IN FRAME FRAME-C                = TRUE.

/* SETTINGS FOR BROWSE BRW_GKOST IN FRAME FRAME-C
   NO-ENABLE                                                            */
/* SETTINGS FOR BROWSE BRW_K4 IN FRAME FRAME-C
   NO-ENABLE                                                            */
ASSIGN 
       BRW_K4:HIDDEN  IN FRAME FRAME-C                = TRUE.

/* SETTINGS FOR BROWSE BRW_KOST IN FRAME FRAME-C
   NO-ENABLE                                                            */
ASSIGN 
       BRW_KOST:HIDDEN  IN FRAME FRAME-C                = TRUE
       BRW_KOST:POPUP-MENU IN FRAME FRAME-C             = MENU POPUP-MENU-BRW_KOST:HANDLE.

/* SETTINGS FOR BROWSE BRW_KOSTMOMS IN FRAME FRAME-C
   NO-ENABLE                                                            */
ASSIGN 
       BRW_KOSTMOMS:HIDDEN  IN FRAME FRAME-C                = TRUE.

/* SETTINGS FOR BROWSE BRW_PLAN IN FRAME FRAME-C
   NO-ENABLE                                                            */
ASSIGN 
       BRW_PLAN:HIDDEN  IN FRAME FRAME-C                = TRUE.

/* SETTINGS FOR BROWSE BRW_TID IN FRAME FRAME-C
   NO-ENABLE                                                            */
ASSIGN 
       BRW_TID:HIDDEN  IN FRAME FRAME-C                = TRUE
       BRW_TID:POPUP-MENU IN FRAME FRAME-C             = MENU POPUP-MENU-BRW_TID:HANDLE
       BRW_TID:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-C = TRUE
       BRW_TID:COLUMN-RESIZABLE IN FRAME FRAME-C       = TRUE.

/* SETTINGS FOR BROWSE BRW_UPP IN FRAME FRAME-C
   NO-ENABLE                                                            */
ASSIGN 
       BRW_UPP:HIDDEN  IN FRAME FRAME-C                = TRUE.

/* SETTINGS FOR BUTTON BTN_BACK IN FRAME FRAME-C
   NO-ENABLE                                                            */
ASSIGN 
       BTN_BACK:HIDDEN IN FRAME FRAME-C           = TRUE.

/* SETTINGS FOR BUTTON BTN_OVER IN FRAME FRAME-C
   NO-ENABLE                                                            */
ASSIGN 
       BTN_OVER:HIDDEN IN FRAME FRAME-C           = TRUE.

/* SETTINGS FOR FILL-IN vfaktplantemp.FAKTNR IN FRAME FRAME-C
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN FILL-IN-TOMDAT IN FRAME FRAME-C
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       FILL-IN-TOMDAT:HIDDEN IN FRAME FRAME-C           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_FDELNR IN FRAME FRAME-C
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN vfaktplantemp.NAMN IN FRAME FRAME-C
   NO-ENABLE                                                            */
/* SETTINGS FOR RADIO-SET RAD_VISA IN FRAME FRAME-C
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       RAD_VISA:HIDDEN IN FRAME FRAME-C           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_MOMS IN FRAME FRAME-C
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-3)
THEN WINDOW-3:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ALLT
/* Query rebuild information for BROWSE BRW_ALLT
     _TblList          = "Temp-Tables.sumpers"
     _OrdList          = "Temp-Tables.sumpers.AONR|yes,Temp-Tables.sumpers.DELNR|yes,Temp-Tables.sumpers.VIBEFATTNING|yes"
     _FldNameList[1]   > Temp-Tables.sumpers.VIBEFATTNING
"sumpers.VIBEFATTNING" "Rubrik" "x(30)" "character" ? ? ? ? ? ? no "" no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.sumpers.AONR
"sumpers.AONR" "Aonr" "X(7)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.sumpers.DELNR
"sumpers.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.sumpers.TIMMAR
"sumpers.TIMMAR" "Antal" "->>>>>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.sumpers.OTIMMAR
"sumpers.OTIMMAR" "A´pris" "->>>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.sumpers.BELOPP
"sumpers.BELOPP" "Summa" "->>>>>>>>>>9" "decimal" ? 12 ? ? 12 ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_ALLT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_FRI
/* Query rebuild information for BROWSE BRW_FRI
     _TblList          = "Temp-Tables.faktfriatemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.faktfriatemp.FAKTTEXT
"faktfriatemp.FAKTTEXT" "Fakt.text" "X(256)" "character" ? ? ? ? ? ? no ? no no "50" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.faktfriatemp.ENHET
"faktfriatemp.ENHET" "Sort" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.faktfriatemp.ANTAL
"faktfriatemp.ANTAL" "Antal" "->>>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.faktfriatemp.PRIS_ENHET
"faktfriatemp.PRIS_ENHET" "Pris/!enhet" "->>>>>>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.faktfriatemp.TOTKALK
"faktfriatemp.TOTKALK" "Tot.kalk" "->>>>>>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.faktfriatemp.TOTALT
"faktfriatemp.TOTALT" "Offert" "->>>>>>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.faktfriatemp.FAKTURERAD
"faktfriatemp.FAKTURERAD" "Ta!med" ? "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[8]   = Temp-Tables.faktfriatemp.AONR
     _FldNameList[9]   > Temp-Tables.faktfriatemp.DELNR
"faktfriatemp.DELNR" "Delnr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_FRI */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_GKOST
/* Query rebuild information for BROWSE BRW_GKOST
     _TblList          = "Temp-Tables.gfaktemp"
     _FldNameList[1]   > Temp-Tables.gfaktemp.TYPTEXT
"gfaktemp.TYPTEXT" ? "X(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.gfaktemp.TOTALT
"gfaktemp.TOTALT" ? ? "integer" ? 12 ? ? 12 ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.gfaktemp.MTRL
"gfaktemp.MTRL" "Materiel!kostnad" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.gfaktemp.OVRIG
"gfaktemp.OVRIG" "Övrig!kostnad" "->>>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.gfaktemp.ARBKOST
"gfaktemp.ARBKOST" "Arbets!kostnad" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.gfaktemp.OBELOPP
"gfaktemp.OBELOPP" "Övertid!kostnad" "->>>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.gfaktemp.TRAKT
"gfaktemp.TRAKT" "Trakt.!kostnad" "->>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > Temp-Tables.gfaktemp.LONTILL
"gfaktemp.LONTILL" "Lönetill.!kostnad" "->>>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[9]   > Temp-Tables.gfaktemp.RES
"gfaktemp.RES" "Res!kostnad" "->>>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[10]   > Temp-Tables.gfaktemp.KBELOPP
"gfaktemp.KBELOPP" "Fr.tjänst!kost." ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_GKOST */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K4
/* Query rebuild information for BROWSE BRW_K4
     _TblList          = "Temp-Tables.momstemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.momstemp.MOMSNR|yes,Temp-Tables.momstemp.MOMSKOD|yes"
     _FldNameList[1]   > Temp-Tables.momstemp.MOMSKOD
"momstemp.MOMSKOD" "Moms!konto" "X(5)" "character" ? ? ? ? ? ? no ? no no "6.5" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.momstemp.MOMSTEXT
"momstemp.MOMSTEXT" "Text" "X(40)" "character" ? ? ? ? ? ? no ? no no "22.5" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.momstemp.MOMSEXTERNT
"momstemp.MOMSEXTERNT" "Moms!i %" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.momstemp.MOMSNR
"momstemp.MOMSNR" "Moms!kod" "X(4)" "character" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_K4 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KOST
/* Query rebuild information for BROWSE BRW_KOST
     _TblList          = "Temp-db.kosttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-db.kosttemp.FAKTNR
"kosttemp.FAKTNR" "Ver-nr" ? "character" ? ? ? ? ? ? yes ? no no "13" yes no no "U" "" ""
     _FldNameList[2]   > Temp-db.kosttemp.BENAMNING
"kosttemp.BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? yes ? no no "17" yes no no "U" "" ""
     _FldNameList[3]   > Temp-db.kosttemp.AONR
"kosttemp.AONR" "Aonr" "X(6)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-db.kosttemp.DELNR
"kosttemp.DELNR" "Del!nr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
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

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KOSTMOMS
/* Query rebuild information for BROWSE BRW_KOSTMOMS
     _TblList          = "Temp-db.kosttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-db.kosttemp.FAKTNR
"kosttemp.FAKTNR" "Ver-nr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-db.kosttemp.BENAMNING
"kosttemp.BENAMNING" "Benämning" "X(22)" "character" ? ? ? ? ? ? no ? no no "17" yes no no "U" "" ""
     _FldNameList[3]   > Temp-db.kosttemp.AONR
"kosttemp.AONR" "Aonr" "X(6)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-db.kosttemp.DELNR
"kosttemp.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-db.kosttemp.MOMSKOD
"kosttemp.MOMSKOD" "Moms!konto" "X(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-db.kosttemp.MOMSEXTERNT
"kosttemp.MOMSEXTERNT" "Moms!i %" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KOSTMOMS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PLAN
/* Query rebuild information for BROWSE BRW_PLAN
     _TblList          = "Temp-Tables.faktstarttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.faktstarttemp.PLAN%
"faktstarttemp.PLAN%" "Andel! i %" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.faktstarttemp.PLANDATUM
"faktstarttemp.PLANDATUM" "Planerat!datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.faktstarttemp.FRITEXT
"faktstarttemp.FRITEXT" "Text" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.faktstarttemp.BELOPP
"faktstarttemp.BELOPP" "Belopp" "->>>>>>9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.faktstarttemp.FAKTURADATUM
"faktstarttemp.FAKTURADATUM" "Fakturerat!datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.faktstarttemp.FAKTURERAD
"faktstarttemp.FAKTURERAD" "Fakturera" ? "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.faktstarttemp.VFAKTNR
"faktstarttemp.VFAKTNR" "Fakturanr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_PLAN */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TID
/* Query rebuild information for BROWSE BRW_TID
     _TblList          = "Temp-db.sumtidtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-db.sumtidtemp.PERSBEF
"sumtidtemp.PERSBEF" "Enhet/Sign!Befattning" "X(23)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" ""
     _FldNameList[2]   > Temp-db.sumtidtemp.DATUM
"sumtidtemp.DATUM" "Datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-db.sumtidtemp.START
"sumtidtemp.START" "Start!tid" ">9.99" "decimal" ? 1 ? ? 1 ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-db.sumtidtemp.SLUT
"sumtidtemp.SLUT" "Slut!tid" ">9.99" "decimal" ? 3 ? ? 3 ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-db.sumtidtemp.TIMMAR
"sumtidtemp.TIMMAR" "Timmar" "->9.9<" "decimal" ? 4 ? ? 4 ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-db.sumtidtemp.BELOPP
"sumtidtemp.BELOPP" "Arbets!kost." "->>>>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-db.sumtidtemp.OTIMMAR
"sumtidtemp.OTIMMAR" "Över.!timmar" "->9.9<" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > Temp-db.sumtidtemp.OBELOPP
"sumtidtemp.OBELOPP" "Övertid!kost." "->>>>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[9]   > Temp-db.sumtidtemp.VITRAKT
"sumtidtemp.VITRAKT" "Lart" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[10]   > Temp-db.sumtidtemp.TRAKTANTAL
"sumtidtemp.TRAKTANTAL" "Antal" "->9.9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[11]   > Temp-db.sumtidtemp.TBELOPP
"sumtidtemp.TBELOPP" "Trakt!kost." "->>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[12]   > Temp-db.sumtidtemp.VILART
"sumtidtemp.VILART" "Lart" "X(7)" "character" ? ? ? ? ? ? no ? no no "4" yes no no "U" "" ""
     _FldNameList[13]   > Temp-db.sumtidtemp.LONTILLANTAL
"sumtidtemp.LONTILLANTAL" "Antal" ">>>9.9<" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[14]   > Temp-db.sumtidtemp.LONKOST
"sumtidtemp.LONKOST" "Löntill!kost." "->>>>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[15]   > Temp-db.sumtidtemp.RESTIM
"sumtidtemp.RESTIM" "Res.!tim." "->9.9<" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[16]   > Temp-db.sumtidtemp.RESKOSTDEC
"sumtidtemp.RESKOSTDEC" "Res.!kost." "->>>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[17]   > Temp-db.sumtidtemp.AONR
"sumtidtemp.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[18]   > Temp-db.sumtidtemp.DELNR
"sumtidtemp.DELNR" "Del!nr" ">99" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[19]   > Temp-db.sumtidtemp.MED
"sumtidtemp.MED" "Ta!med" "Ja/Nej" "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_TID */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UPP
/* Query rebuild information for BROWSE BRW_UPP
     _TblList          = "Temp-Tables.faktupparbtemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.faktupparbtemp.UPPARB%|yes"
     _FldNameList[1]   > Temp-Tables.faktupparbtemp.AONR
"faktupparbtemp.AONR" "Aonr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.faktupparbtemp.DELNR
"faktupparbtemp.DELNR" "Del!nr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.faktupparbtemp.UPLAN%
"faktupparbtemp.UPLAN%" "Plan.!upparb.%" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.faktupparbtemp.UPPARB%
"faktupparbtemp.UPPARB%" "Upparb.%" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.faktupparbtemp.UPPBELOPP
"faktupparbtemp.UPPBELOPP" "Upparbetat!belopp" "->>>>>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.faktupparbtemp.FAKT%
"faktupparbtemp.FAKT%" "Fakt.%" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.faktupparbtemp.FAKTBELOPP
"faktupparbtemp.FAKTBELOPP" "Fakturera!belopp" "->>>>>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > Temp-Tables.faktupparbtemp.FRITEXT
"faktupparbtemp.FRITEXT" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[9]   = Temp-Tables.faktupparbtemp.KRITERIUM
     _FldNameList[10]   > Temp-Tables.faktupparbtemp.FAKTURERAD
"faktupparbtemp.FAKTURERAD" "Fakturerad" ? "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[11]   > Temp-Tables.faktupparbtemp.FAKTURADATUM
"faktupparbtemp.FAKTURADATUM" "Faktura!datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_UPP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-C
/* Query rebuild information for FRAME FRAME-C
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-C */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_FRI
&Scoped-define SELF-NAME BRW_FRI
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_FRI WINDOW-3
ON MOUSE-SELECT-DBLCLICK OF BRW_FRI IN FRAME FRAME-C /* Fri komplettering av faktura */
DO: 
   
   RUN brwfri_UI (INPUT FALSE).
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_FRI WINDOW-3
ON ROW-DISPLAY OF BRW_FRI IN FRAME FRAME-C /* Fri komplettering av faktura */
DO:
   IF faktfriatemp.TYP = "" THEN DO:
      ASSIGN
      faktfriatemp.FAKTTEXT:FGCOLOR IN BROWSE BRW_FRI = 12
      faktfriatemp.ENHET:FGCOLOR IN BROWSE BRW_FRI = 12
      faktfriatemp.ANTAL:FGCOLOR IN BROWSE BRW_FRI = 12
      faktfriatemp.PRIS_ENHET:FGCOLOR IN BROWSE BRW_FRI = 12
      faktfriatemp.TOTKALK:FGCOLOR IN BROWSE BRW_FRI = 12
      faktfriatemp.TOTALT:FGCOLOR IN BROWSE BRW_FRI = 12
      faktfriatemp.FAKTURERAD:FGCOLOR IN BROWSE BRW_FRI = 12.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_FRI WINDOW-3
ON VALUE-CHANGED OF BRW_FRI IN FRAME FRAME-C /* Fri komplettering av faktura */
DO:
   musz = musz.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K4
&Scoped-define SELF-NAME BRW_K4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K4 WINDOW-3
ON MOUSE-SELECT-DBLCLICK OF BRW_K4 IN FRAME FRAME-C /* Momstabell */
DO:
   /*RUN andra_UI.*/
   APPLY "CHOOSE" TO BTN_OVER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K4 WINDOW-3
ON VALUE-CHANGED OF BRW_K4 IN FRAME FRAME-C /* Momstabell */
DO:
   /*DISPLAY MOMSTAB.MOMSEXTERNT MOMSTAB.MOMSKOD MOMSTAB.MOMSNR WITH FRAME {&FRAME-NAME}.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KOST
&Scoped-define SELF-NAME BRW_KOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KOST WINDOW-3
ON MOUSE-SELECT-DBLCLICK OF BRW_KOST IN FRAME FRAME-C /* Kostnadsregistrering - "Klicka" på de som ej ska faktureras i MED-fältet */
DO:
   RUN medkost_UI (INPUT 1).      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KOST WINDOW-3
ON ROW-LEAVE OF BRW_KOST IN FRAME FRAME-C /* Kostnadsregistrering - "Klicka" på de som ej ska faktureras i MED-fältet */
DO:
   IF AVAILABLE kosttemp THEN DO:
      DISPLAY  
               kosttemp.BENAMNING  
               kosttemp.BOKKONTO   
               
               kosttemp.FAKTNR     
               kosttemp.FRTJPAKR   
               kosttemp.MASKKOST   
               kosttemp.MED              
               kosttemp.MTRL       
               kosttemp.MTRLPAKR   
               kosttemp.OVRKR      
               kosttemp.PERSKOST   
               kosttemp.TRAKTKOST  
         WITH BROWSE BRW_KOST.
      
      ASSIGN
      kosttemp.BENAMNING    = INPUT BROWSE BRW_KOST kosttemp.BENAMNING   
      kosttemp.BOKKONTO     = INPUT BROWSE BRW_KOST kosttemp.BOKKONTO    
      kosttemp.FAKTNR       = INPUT BROWSE BRW_KOST kosttemp.FAKTNR      
      kosttemp.FRTJPAKR     = INPUT BROWSE BRW_KOST kosttemp.FRTJPAKR    
      kosttemp.MASKKOST     = INPUT BROWSE BRW_KOST kosttemp.MASKKOST    
      kosttemp.MED          = INPUT BROWSE BRW_KOST kosttemp.MED                     
      kosttemp.MTRL         = INPUT BROWSE BRW_KOST kosttemp.MTRL        
      kosttemp.MTRLPAKR     = INPUT BROWSE BRW_KOST kosttemp.MTRLPAKR    
      kosttemp.OVRKR        = INPUT BROWSE BRW_KOST kosttemp.OVRKR       
      kosttemp.PERSKOST     = INPUT BROWSE BRW_KOST kosttemp.PERSKOST    
      kosttemp.TRAKTKOST    = INPUT BROWSE BRW_KOST kosttemp.TRAKTKOST.   
      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kosttemp.FAKTNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kosttemp.FAKTNR BRW_KOST _BROWSE-COLUMN WINDOW-3
ON ENTRY OF kosttemp.FAKTNR IN BROWSE BRW_KOST /* Ver-nr */
DO:
  DISPLAY kosttemp.FAKTNR WITH BROWSE BRW_KOST.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kosttemp.FAKTNR BRW_KOST _BROWSE-COLUMN WINDOW-3
ON LEAVE OF kosttemp.FAKTNR IN BROWSE BRW_KOST /* Ver-nr */
DO:
   IF kosttemp.FAKTNR NE INPUT BROWSE BRW_KOST kosttemp.FAKTNR THEN DO:
      IF TOG_MOMS = TRUE THEN DO:
         RUN kontokoll_UI.
      END.
      ELSE DO:
         kosttemp.FAKTNR = INPUT BROWSE BRW_KOST kosttemp.FAKTNR.   
      END.
      DISPLAY kosttemp.FAKTNR WITH BROWSE BRW_KOST.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kosttemp.BENAMNING
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kosttemp.BENAMNING BRW_KOST _BROWSE-COLUMN WINDOW-3
ON ENTRY OF kosttemp.BENAMNING IN BROWSE BRW_KOST /* Benämning */
DO:
   IF AVAILABLE kosttemp THEN
   DISPLAY kosttemp.BENAMNING WITH BROWSE BRW_KOST.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kosttemp.BENAMNING BRW_KOST _BROWSE-COLUMN WINDOW-3
ON LEAVE OF kosttemp.BENAMNING IN BROWSE BRW_KOST /* Benämning */
DO:
   IF AVAILABLE kosttemp THEN DO:
   
      IF kosttemp.BENAMNING NE INPUT BROWSE BRW_KOST kosttemp.BENAMNING THEN DO:
         IF TOG_MOMS = TRUE THEN DO:
            RUN kontokoll_UI.
         END.
         ELSE DO:
            kosttemp.BENAMNING = INPUT BROWSE BRW_KOST kosttemp.BENAMNING.   
         END. 
         kosttemp.BENAMNING = INPUT BROWSE BRW_KOST kosttemp.BENAMNING.
         DISPLAY kosttemp.BENAMNING WITH BROWSE BRW_KOST.
      END.
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kosttemp.TRAKTKOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kosttemp.TRAKTKOST BRW_KOST _BROWSE-COLUMN WINDOW-3
ON ENTRY OF kosttemp.TRAKTKOST IN BROWSE BRW_KOST /* Trakt.!kost. */
DO:
   IF AVAILABLE kosttemp THEN
   DISPLAY kosttemp.TRAKTKOST WITH BROWSE BRW_KOST.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kosttemp.TRAKTKOST BRW_KOST _BROWSE-COLUMN WINDOW-3
ON LEAVE OF kosttemp.TRAKTKOST IN BROWSE BRW_KOST /* Trakt.!kost. */
DO:
   IF AVAILABLE kosttemp THEN DO:
      IF kosttemp.TRAKTKOST = INPUT BROWSE BRW_KOST kosttemp.TRAKTKOST THEN musz = musz.
      ELSE DO:
         IF TOG_MOMS = TRUE THEN DO:
            RUN kontokoll_UI.
         END.
         ELSE DO:
            DEFINE VARIABLE gamla AS DECIMAL NO-UNDO.
            gamla = kosttemp.TRAKTKOST.
            kosttemp.TRAKTKOST = INPUT BROWSE BRW_KOST kosttemp.TRAKTKOST.   
            RUN findg3_UI (INPUT 4, INPUT 0, INPUT 0, INPUT 0,INPUT 0, 
                           INPUT gamla, INPUT kosttemp.TRAKTKOST, INPUT 0, INPUT 0, kosttemp.MED).
        END.
         DISPLAY kosttemp.TRAKTKOST WITH BROWSE BRW_KOST.
      END.             
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kosttemp.PERSKOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kosttemp.PERSKOST BRW_KOST _BROWSE-COLUMN WINDOW-3
ON ENTRY OF kosttemp.PERSKOST IN BROWSE BRW_KOST /* Pers.!kost */
DO:
   IF AVAILABLE kosttemp THEN
   DISPLAY kosttemp.PERSKOST WITH BROWSE BRW_KOST.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kosttemp.PERSKOST BRW_KOST _BROWSE-COLUMN WINDOW-3
ON LEAVE OF kosttemp.PERSKOST IN BROWSE BRW_KOST /* Pers.!kost */
DO:
   IF AVAILABLE kosttemp THEN DO:
      IF kosttemp.PERSKOST = INPUT BROWSE BRW_KOST kosttemp.PERSKOST THEN musz = musz.
      ELSE DO: 
         IF TOG_MOMS = TRUE THEN DO:
            RUN kontokoll_UI.
         END.
         ELSE DO:
            DEFINE VARIABLE gamla AS DECIMAL NO-UNDO.
            gamla = kosttemp.PERSKOST.
            kosttemp.PERSKOST = INPUT BROWSE BRW_KOST kosttemp.PERSKOST.      
            RUN findg3_UI (INPUT 3, INPUT 0, INPUT 0, INPUT 0,INPUT 0, 
                           INPUT gamla, INPUT kosttemp.PERSKOST, INPUT 0, INPUT 0, kosttemp.MED).
         END.                  
         DISPLAY kosttemp.PERSKOST WITH BROWSE BRW_KOST.
      END.   
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kosttemp.MASKKOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kosttemp.MASKKOST BRW_KOST _BROWSE-COLUMN WINDOW-3
ON ENTRY OF kosttemp.MASKKOST IN BROWSE BRW_KOST /* Fr.tjänst!kost. */
DO:
   IF AVAILABLE kosttemp THEN
   DISPLAY kosttemp.MASKKOST WITH BROWSE BRW_KOST.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kosttemp.MASKKOST BRW_KOST _BROWSE-COLUMN WINDOW-3
ON LEAVE OF kosttemp.MASKKOST IN BROWSE BRW_KOST /* Fr.tjänst!kost. */
DO: 
   IF AVAILABLE kosttemp THEN DO:
      IF kosttemp.MASKKOST = INPUT BROWSE BRW_KOST kosttemp.MASKKOST THEN musz = musz.
      ELSE DO:
         IF TOG_MOMS = TRUE THEN DO:
            RUN kontokoll_UI.
         END.
         ELSE DO:
            DEFINE VARIABLE gamla AS DECIMAL NO-UNDO.
            gamla = kosttemp.MASKKOST.
            kosttemp.MASKKOST = INPUT BROWSE BRW_KOST kosttemp.MASKKOST.
            DISPLAY kosttemp.MASKKOST WITH BROWSE BRW_KOST.
            kosttemp.FRTJPAKR = kosttemp.MASKKOST * kundregeltemp.FRTJPA / 100.
            DISPLAY kosttemp.FRTJPAKR WITH BROWSE BRW_KOST.     
            RUN findg3_UI (INPUT 5, INPUT 0, INPUT 0, INPUT 0,INPUT 0, 
                           INPUT gamla, INPUT kosttemp.MASKKOST, INPUT 0, INPUT 0, kosttemp.MED). 
         END.
         DISPLAY kosttemp.MASKKOST WITH BROWSE BRW_KOST.                     
      END.   
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kosttemp.MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kosttemp.MTRL BRW_KOST _BROWSE-COLUMN WINDOW-3
ON ENTRY OF kosttemp.MTRL IN BROWSE BRW_KOST /* Mtrl.!kr */
DO:
   IF AVAILABLE kosttemp THEN
   DISPLAY kosttemp.MTRL WITH BROWSE BRW_KOST.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kosttemp.MTRL BRW_KOST _BROWSE-COLUMN WINDOW-3
ON LEAVE OF kosttemp.MTRL IN BROWSE BRW_KOST /* Mtrl.!kr */
DO:
   IF AVAILABLE kosttemp THEN DO:
      IF kosttemp.MTRL = INPUT BROWSE BRW_KOST kosttemp.MTRL THEN musz = musz.
      ELSE DO:   
         IF TOG_MOMS = TRUE THEN DO:
            RUN kontokoll_UI.
         END.
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
         END.                  
         DISPLAY kosttemp.MTRL WITH BROWSE BRW_KOST.
      END. 
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kosttemp.OVRKR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kosttemp.OVRKR BRW_KOST _BROWSE-COLUMN WINDOW-3
ON ENTRY OF kosttemp.OVRKR IN BROWSE BRW_KOST /* Övrig!kostnad */
DO:
   IF AVAILABLE kosttemp THEN
   DISPLAY kosttemp.OVRKR WITH BROWSE BRW_KOST.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kosttemp.OVRKR BRW_KOST _BROWSE-COLUMN WINDOW-3
ON LEAVE OF kosttemp.OVRKR IN BROWSE BRW_KOST /* Övrig!kostnad */
DO:
   IF AVAILABLE kosttemp THEN DO:
      IF kosttemp.OVRKR = INPUT BROWSE BRW_KOST kosttemp.OVRKR THEN musz = musz.
      ELSE DO:
         IF TOG_MOMS = TRUE THEN DO:
            RUN kontokoll_UI.
         END.
         ELSE DO:
            DEFINE VARIABLE gamla AS DECIMAL NO-UNDO.
            gamla = kosttemp.OVRKR.
            kosttemp.OVRKR = INPUT BROWSE BRW_KOST kosttemp.OVRKR.
            DISPLAY kosttemp.OVRKR WITH BROWSE BRW_KOST.
            RUN findg3_UI (INPUT 2, INPUT 0, INPUT 0, INPUT 0,INPUT 0, 
                           INPUT gamla, INPUT kosttemp.OVRKR, INPUT 0, INPUT 0, kosttemp.MED).
         END.  
         DISPLAY kosttemp.OVRKR WITH BROWSE BRW_KOST.
      END.      
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kosttemp.MED
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kosttemp.MED BRW_KOST _BROWSE-COLUMN WINDOW-3
ON LEAVE OF kosttemp.MED IN BROWSE BRW_KOST /* Ta!med */
DO:
   /*JA NEJ ?*/
   IF kosttemp.MED NE INPUT BROWSE BRW_KOST kosttemp.MED THEN DO:       
      RUN medkost_UI (INPUT 2).  
   END.   
   {musarrow.i}
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


&Scoped-define BROWSE-NAME BRW_KOSTMOMS
&Scoped-define SELF-NAME BRW_KOSTMOMS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KOSTMOMS WINDOW-3
ON VALUE-CHANGED OF BRW_KOSTMOMS IN FRAME FRAME-C /* Kostnadsregistrering - "Markera de som skall ha annan momssats" */
DO:
   musz = musz.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_PLAN
&Scoped-define SELF-NAME BRW_PLAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PLAN WINDOW-3
ON MOUSE-SELECT-DBLCLICK OF BRW_PLAN IN FRAME FRAME-C /* Faktureringstidpunkter */
DO:              
   RUN medplan_UI (INPUT 1, INPUT "PLAN" ).      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PLAN WINDOW-3
ON VALUE-CHANGED OF BRW_PLAN IN FRAME FRAME-C /* Faktureringstidpunkter */
DO:
   status-ok = BRW_PLAN:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME faktstarttemp.FRITEXT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktstarttemp.FRITEXT BRW_PLAN _BROWSE-COLUMN WINDOW-3
ON ENTRY OF faktstarttemp.FRITEXT IN BROWSE BRW_PLAN /* Text */
DO:
   IF AVAILABLE faktstarttemp THEN
   DISPLAY faktstarttemp.FRITEXT WITH BROWSE BRW_PLAN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktstarttemp.FRITEXT BRW_PLAN _BROWSE-COLUMN WINDOW-3
ON LEAVE OF faktstarttemp.FRITEXT IN BROWSE BRW_PLAN /* Text */
DO:
   IF AVAILABLE faktstarttemp THEN DO:
      IF faktstarttemp.FRITEXT NE INPUT BROWSE BRW_PLAN faktstarttemp.FRITEXT THEN DO:
         IF TOG_MOMS = TRUE THEN DO:   
            RUN kontokoll_UI.
         END.   
         ELSE IF faktstarttemp.VFAKTNR NE 0 THEN DO:
            MESSAGE "Det går inte att ändra på denna post då den redan är fakturerad."   
            VIEW-AS ALERT-BOX.
         END.
         ELSE DO:   
            faktstarttemp.FRITEXT = INPUT BROWSE BRW_PLAN faktstarttemp.FRITEXT.
            DISPLAY faktstarttemp.FRITEXT WITH BROWSE BRW_PLAN.         
         END.
         APPLY "ENTRY" TO faktstarttemp.FRITEXT IN BROWSE BRW_PLAN.
         DISPLAY faktstarttemp.FRITEXT WITH BROWSE BRW_PLAN.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME faktstarttemp.BELOPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktstarttemp.BELOPP BRW_PLAN _BROWSE-COLUMN WINDOW-3
ON ENTRY OF faktstarttemp.BELOPP IN BROWSE BRW_PLAN /* Belopp */
DO:
   RUN medplan_UI (INPUT 1, INPUT "PLAN").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktstarttemp.BELOPP BRW_PLAN _BROWSE-COLUMN WINDOW-3
ON LEAVE OF faktstarttemp.BELOPP IN BROWSE BRW_PLAN /* Belopp */
DO:
   RUN beloppleav_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME faktstarttemp.FAKTURERAD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktstarttemp.FAKTURERAD BRW_PLAN _BROWSE-COLUMN WINDOW-3
ON LEAVE OF faktstarttemp.FAKTURERAD IN BROWSE BRW_PLAN /* Fakturera */
DO:
   IF INPUT BROWSE BRW_PLAN faktstarttemp.FAKTURERAD = ? THEN DO:
      MESSAGE "Ogiltigt värde!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF faktstarttemp.FAKTURERAD NE INPUT BROWSE BRW_PLAN faktstarttemp.FAKTURERAD THEN RUN medplan_UI (INPUT 2, INPUT "PLAN" ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktstarttemp.FAKTURERAD BRW_PLAN _BROWSE-COLUMN WINDOW-3
ON MOUSE-SELECT-CLICK OF faktstarttemp.FAKTURERAD IN BROWSE BRW_PLAN /* Fakturera */
DO:
   RUN medplan_UI (INPUT 1, INPUT "PLAN").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_TID
&Scoped-define SELF-NAME BRW_TID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_TID WINDOW-3
ON MOUSE-MENU-CLICK OF BRW_TID IN FRAME FRAME-C /* Tidskrivning under perioden - "Klicka" på de som ej ska faktureras i MED-fältet */
DO:  
  RUN brwtid_UI.   
  {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_TID WINDOW-3
ON MOUSE-SELECT-DBLCLICK OF BRW_TID IN FRAME FRAME-C /* Tidskrivning under perioden - "Klicka" på de som ej ska faktureras i MED-fältet */
DO:   
   RUN medtid_UI (INPUT 1).  
       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_TID WINDOW-3
ON ROW-DISPLAY OF BRW_TID IN FRAME FRAME-C /* Tidskrivning under perioden - "Klicka" på de som ej ska faktureras i MED-fältet */
DO:
    
   IF {&WINDOW-NAME}:WIDTH-CHARS = {&WINDOW-NAME}:MAX-WIDTH-CHARS THEN DO:    
      BRW_TID:FONT IN FRAME {&FRAME-NAME} = 0.     
   END.
   ELSE DO:
      BRW_TID:FONT IN FRAME {&FRAME-NAME} = 11.    
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sumtidtemp.START
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sumtidtemp.START BRW_TID _BROWSE-COLUMN WINDOW-3
ON ENTRY OF sumtidtemp.START IN BROWSE BRW_TID /* Start!tid */
DO:
   IF AVAILABLE sumtidtemp THEN
   DISPLAY sumtidtemp.START WITH BROWSE BRW_TID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sumtidtemp.START BRW_TID _BROWSE-COLUMN WINDOW-3
ON LEAVE OF sumtidtemp.START IN BROWSE BRW_TID /* Start!tid */
DO:
   IF AVAILABLE sumtidtemp THEN DO:
      IF sumtidtemp.START = INPUT BROWSE BRW_TID sumtidtemp.START THEN musz = musz.
      ELSE DO:
         IF TOG_MOMS = TRUE THEN DO:
            RUN kontokoll_UI.
         END.
         ELSE DO:
            DEFINE VARIABLE gamla AS DECIMAL NO-UNDO.
            gamla = sumtidtemp.BELOPP.
            sumtidtemp.START = INPUT BROWSE BRW_TID sumtidtemp.START.   
            IF sumtidtemp.START >= sumtidtemp.SLUT THEN DO:
               MESSAGE "Starttiden kan inte vara större än sluttiden!" VIEW-AS ALERT-BOX.      
               RETURN NO-APPLY.
            END.
            IF sumtidtemp.START < 24.00 THEN DO:      
               DISPLAY sumtidtemp.START WITH BROWSE BRW_TID.
               IF sumtidtemp.OTIMMAR = 0 AND sumtidtemp.RESTIM = 0 THEN DO:
                  sumtidtemp.TIMMAR = (klockan100(sumtidtemp.SLUT) - klockan100(sumtidtemp.START)) - sumtidtemp.LUNCH.            
                  sumtidtemp.BELOPP = sumtidtemp.TIMMAR * sumtidtemp.PRISA.
                  DISPLAY sumtidtemp.BELOPP sumtidtemp.TIMMAR WITH BROWSE BRW_TID.
                  RUN findg3_UI (INPUT 0, INPUT gamla, INPUT sumtidtemp.BELOPP, INPUT 0,INPUT 0, 
                                 INPUT 0, INPUT 0, INPUT 0, INPUT 0, sumtidtemp.MED).
               END.
            END.
            ELSE DO: 
               MESSAGE "Klockan kan inte vara större än 24.00!" VIEW-AS ALERT-BOX.      
               RETURN NO-APPLY.
            END.
         END.   
         DISPLAY sumtidtemp.START WITH BROWSE BRW_TID.
      END.
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sumtidtemp.SLUT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sumtidtemp.SLUT BRW_TID _BROWSE-COLUMN WINDOW-3
ON ENTRY OF sumtidtemp.SLUT IN BROWSE BRW_TID /* Slut!tid */
DO:
   IF AVAILABLE sumtidtemp THEN
   DISPLAY sumtidtemp.SLUT WITH BROWSE BRW_TID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sumtidtemp.SLUT BRW_TID _BROWSE-COLUMN WINDOW-3
ON LEAVE OF sumtidtemp.SLUT IN BROWSE BRW_TID /* Slut!tid */
DO:
   IF sumtidtemp.SLUT = INPUT BROWSE BRW_TID sumtidtemp.SLUT THEN musz = musz.
   ELSE DO:
      IF TOG_MOMS = TRUE THEN DO:
         RUN kontokoll_UI.
      END.
      ELSE DO:
         DEFINE VARIABLE gamla AS DECIMAL NO-UNDO.
         gamla = sumtidtemp.BELOPP.
         IF INPUT BROWSE BRW_TID sumtidtemp.SLUT <= sumtidtemp.START THEN DO:
            MESSAGE "Sluttiden kan inte vara mindre än starttiden! "  VIEW-AS ALERT-BOX.
            DISPLAY sumtidtemp.SLUT WITH BROWSE BRW_TID.
            RETURN NO-APPLY.
         END.
         IF INPUT BROWSE BRW_TID sumtidtemp.SLUT > 24.00 THEN DO:
            MESSAGE "Klockan kan inte vara större än 24.00!" VIEW-AS ALERT-BOX.
            DISPLAY sumtidtemp.SLUT WITH BROWSE BRW_TID.
            RETURN NO-APPLY.
         END.
         ELSE DO: 
            sumtidtemp.SLUT = INPUT BROWSE BRW_TID sumtidtemp.SLUT.        
            DISPLAY sumtidtemp.SLUT WITH BROWSE BRW_TID.
            IF sumtidtemp.OTIMMAR = 0 AND sumtidtemp.RESTIM = 0 THEN DO:
               sumtidtemp.TIMMAR = (klockan100(sumtidtemp.SLUT) - klockan100(sumtidtemp.START)) - sumtidtemp.LUNCH.
               sumtidtemp.BELOPP = sumtidtemp.TIMMAR * sumtidtemp.PRISA.
               DISPLAY sumtidtemp.BELOPP sumtidtemp.TIMMAR WITH BROWSE BRW_TID.
               RUN findg3_UI (INPUT 0, INPUT gamla, INPUT sumtidtemp.BELOPP, INPUT 0,INPUT 0, 
                              INPUT 0, INPUT 0, INPUT 0, INPUT 0, sumtidtemp.MED). 
            END.
         END.  
      END.   
      DISPLAY sumtidtemp.SLUT WITH BROWSE BRW_TID.
   END.   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sumtidtemp.TIMMAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sumtidtemp.TIMMAR BRW_TID _BROWSE-COLUMN WINDOW-3
ON ENTRY OF sumtidtemp.TIMMAR IN BROWSE BRW_TID /* Timmar */
DO:
   IF AVAILABLE sumtidtemp THEN
   DISPLAY sumtidtemp.TIMMAR WITH BROWSE BRW_TID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sumtidtemp.TIMMAR BRW_TID _BROWSE-COLUMN WINDOW-3
ON LEAVE OF sumtidtemp.TIMMAR IN BROWSE BRW_TID /* Timmar */
DO:
   RUN sumtimmar_UI.      
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sumtidtemp.OTIMMAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sumtidtemp.OTIMMAR BRW_TID _BROWSE-COLUMN WINDOW-3
ON ENTRY OF sumtidtemp.OTIMMAR IN BROWSE BRW_TID /* Över.!timmar */
DO:
   IF AVAILABLE sumtidtemp THEN
   DISPLAY sumtidtemp.OTIMMAR WITH BROWSE BRW_TID.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sumtidtemp.OTIMMAR BRW_TID _BROWSE-COLUMN WINDOW-3
ON LEAVE OF sumtidtemp.OTIMMAR IN BROWSE BRW_TID /* Över.!timmar */
DO:
   IF sumtidtemp.OTIMMAR = INPUT BROWSE BRW_TID sumtidtemp.OTIMMAR THEN musz = musz.
   ELSE DO:
      IF TOG_MOMS = TRUE THEN DO:
         RUN kontokoll_UI.
      END.
      ELSE DO:
         DEFINE VARIABLE gamla AS DECIMAL NO-UNDO.
         gamla = sumtidtemp.OBELOPP.   
         IF sumtidtemp.TIMMAR = 0 AND sumtidtemp.RESTIM = 0 THEN DO:
            IF sumtidtemp.OTIMMAR NE 0 THEN resber = sumtidtemp.OBELOPP / sumtidtemp.OTIMMAR.   
            sumtidtemp.OTIMMAR = INPUT BROWSE BRW_TID sumtidtemp.OTIMMAR.   
            DISPLAY sumtidtemp.OTIMMAR WITH BROWSE BRW_TID.   
            sumtidtemp.OBELOPP = resber * sumtidtemp.OTIMMAR.
            sumtidtemp.OPRIS = resber.
            DISPLAY sumtidtemp.OBELOPP WITH BROWSE BRW_TID.
            RUN findg3_UI (INPUT 0, INPUT 0, INPUT 0, INPUT gamla,INPUT sumtidtemp.OBELOPP, 
                           INPUT 0, INPUT 0, INPUT 0, INPUT 0, sumtidtemp.MED). 
         END.
         ELSE sumtidtemp.OTIMMAR = 0.            
      END.   
      DISPLAY sumtidtemp.OTIMMAR WITH BROWSE BRW_TID.
   END.   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sumtidtemp.RESTIM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sumtidtemp.RESTIM BRW_TID _BROWSE-COLUMN WINDOW-3
ON ENTRY OF sumtidtemp.RESTIM IN BROWSE BRW_TID /* Res.!tim. */
DO:
   IF AVAILABLE sumtidtemp THEN
   DISPLAY sumtidtemp.RESTIM WITH BROWSE BRW_TID.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sumtidtemp.RESTIM BRW_TID _BROWSE-COLUMN WINDOW-3
ON LEAVE OF sumtidtemp.RESTIM IN BROWSE BRW_TID /* Res.!tim. */
DO:
   IF sumtidtemp.RESTIM = INPUT BROWSE BRW_TID sumtidtemp.RESTIM THEN musz = musz.
   ELSE DO:
      IF TOG_MOMS = TRUE THEN DO:
         RUN kontokoll_UI.
      END.
      ELSE DO:
         DEFINE VARIABLE gamla AS DECIMAL NO-UNDO.
         gamla = sumtidtemp.RESKOSTDEC.
         IF sumtidtemp.TIMMAR = 0 AND sumtidtemp.OTIMMAR = 0 THEN DO:
            IF sumtidtemp.RESTIM NE 0 THEN resber = sumtidtemp.RESKOSTDEC / sumtidtemp.RESTIM.   
            sumtidtemp.RESTIM = INPUT BROWSE BRW_TID sumtidtemp.RESTIM.   
            DISPLAY sumtidtemp.RESTIM WITH BROWSE BRW_TID.   
            sumtidtemp.RESKOSTDEC = resber * sumtidtemp.RESTIM.
            sumtidtemp.RESPRIS = resber.
            DISPLAY sumtidtemp.RESKOSTDEC WITH BROWSE BRW_TID.
            RUN findg3_UI (INPUT 0, INPUT 0, INPUT 0, INPUT 0,INPUT 0, 
                           INPUT 0, INPUT 0, INPUT gamla, INPUT sumtidtemp.RESKOSTDEC, sumtidtemp.MED). 
         END.
         ELSE sumtidtemp.RESTIM = 0.   
      END.
      DISPLAY sumtidtemp.RESTIM WITH BROWSE BRW_TID.   
   END.   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sumtidtemp.MED
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sumtidtemp.MED BRW_TID _BROWSE-COLUMN WINDOW-3
ON LEAVE OF sumtidtemp.MED IN BROWSE BRW_TID /* Ta!med */
DO:
   IF sumtidtemp.MED NE INPUT BROWSE BRW_TID sumtidtemp.MED THEN RUN medtid_UI (INPUT 2).
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sumtidtemp.MED BRW_TID _BROWSE-COLUMN WINDOW-3
ON MOUSE-SELECT-CLICK OF sumtidtemp.MED IN BROWSE BRW_TID /* Ta!med */
DO:
   RUN medtid_UI (INPUT 1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_UPP
&Scoped-define SELF-NAME BRW_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_UPP WINDOW-3
ON MOUSE-SELECT-DBLCLICK OF BRW_UPP IN FRAME FRAME-C /* Upparbetade kostnader */
DO:
   RUN medplan_UI (INPUT 1, INPUT "UPP").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_UPP WINDOW-3
ON ROW-DISPLAY OF BRW_UPP IN FRAME FRAME-C /* Upparbetade kostnader */
DO:
   IF faktupparbtemp.FAKTBELOPP NE 0 THEN DO:
      IF faktupparbtemp.FAKTURERAD = FALSE THEN DO:
         faktupparbtemp.FAKTURERAD:FGCOLOR IN BROWSE BRW_UPP = 12. 
      END.
   END.      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_UPP WINDOW-3
ON VALUE-CHANGED OF BRW_UPP IN FRAME FRAME-C /* Upparbetade kostnader */
DO:
   status-ok = BRW_UPP:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME faktupparbtemp.FRITEXT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktupparbtemp.FRITEXT BRW_UPP _BROWSE-COLUMN WINDOW-3
ON ENTRY OF faktupparbtemp.FRITEXT IN BROWSE BRW_UPP /* Text */
DO:
   IF AVAILABLE faktupparbtemp THEN
   DISPLAY faktupparbtemp.FRITEXT WITH BROWSE BRW_UPP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktupparbtemp.FRITEXT BRW_UPP _BROWSE-COLUMN WINDOW-3
ON LEAVE OF faktupparbtemp.FRITEXT IN BROWSE BRW_UPP /* Text */
DO:
   IF faktupparbtemp.FRITEXT NE INPUT BROWSE BRW_UPP faktupparbtemp.FRITEXT THEN DO:
      IF TOG_MOMS = TRUE THEN DO:   
         RUN kontokoll_UI.
      END.   
      ELSE IF faktupparbtemp.VFAKTNR NE 0 THEN DO:
         MESSAGE "Det går inte att ändra på denna post då den redan är fakturerad."   
         VIEW-AS ALERT-BOX.
      END.
      ELSE DO:         
         faktupparbtemp.FRITEXT = INPUT BROWSE BRW_UPP faktupparbtemp.FRITEXT.
         DISPLAY faktupparbtemp.FRITEXT WITH BROWSE BRW_UPP.         
      END.
      APPLY "ENTRY" TO faktupparbtemp.FRITEXT IN BROWSE BRW_UPP.
      DISPLAY faktupparbtemp.FRITEXT WITH BROWSE BRW_UPP.
   END. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME faktupparbtemp.FAKTURERAD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktupparbtemp.FAKTURERAD BRW_UPP _BROWSE-COLUMN WINDOW-3
ON ENTRY OF faktupparbtemp.FAKTURERAD IN BROWSE BRW_UPP /* Fakturerad */
DO:
   IF AVAILABLE faktupparbtemp THEN
   DISPLAY faktupparbtemp.FAKTURERAD WITH BROWSE BRW_UPP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktupparbtemp.FAKTURERAD BRW_UPP _BROWSE-COLUMN WINDOW-3
ON LEAVE OF faktupparbtemp.FAKTURERAD IN BROWSE BRW_UPP /* Fakturerad */
DO:
   IF INPUT BROWSE BRW_UPP faktupparbtemp.FAKTURERAD = ? THEN DO:
      MESSAGE "Ogiltigt värde!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF faktupparbtemp.FAKTURERAD NE INPUT BROWSE BRW_UPP faktupparbtemp.FAKTURERAD THEN 
   RUN medplan_UI (INPUT 2, INPUT "UPP"). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktupparbtemp.FAKTURERAD BRW_UPP _BROWSE-COLUMN WINDOW-3
ON MOUSE-SELECT-CLICK OF faktupparbtemp.FAKTURERAD IN BROWSE BRW_UPP /* Fakturerad */
DO:
   RUN medplan_UI (INPUT 1, INPUT "UPP").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-3
ON CHOOSE OF BTN_AVB IN FRAME FRAME-C /* Avbryt */
DO:
   {muswait.i}
   IF TOG_MOMS = TRUE THEN DO:
      RUN avb_UI.
   END.
   ELSE DO:
      MESSAGE "Vill du spara?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE sp1 AS LOGICAL.
      CASE sp1:
         WHEN TRUE THEN DO:
            IF skarpvar = 0 THEN RUN spara_UI (INPUT skarpvar).
            RUN avb_UI.  
         END.
         WHEN FALSE THEN DO:
            RUN avb_UI.
         END.
      END CASE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BACK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BACK WINDOW-3
ON CHOOSE OF BTN_BACK IN FRAME FRAME-C
DO:   
   RUN back_UI.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FRIAND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FRIAND WINDOW-3
ON CHOOSE OF BTN_FRIAND IN FRAME FRAME-C /* Ändra fripost */
DO:   
   RUN brwfri_UI (INPUT FALSE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FRIBORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FRIBORT WINDOW-3
ON CHOOSE OF BTN_FRIBORT IN FRAME FRAME-C /* Ta bort fripost */
DO:   
   RUN fribort_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FRINY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FRINY WINDOW-3
ON CHOOSE OF BTN_FRINY IN FRAME FRAME-C /* Ny */
DO:   
   RUN brwfri_UI (INPUT TRUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK WINDOW-3
ON CHOOSE OF BTN_OK IN FRAME FRAME-C /* Ok */
DO:   
   slutvar = 0.
   IF TOG_MOMS = TRUE THEN DO:
      RUN avb_UI.
   END.
   ELSE DO:
      
      IF skarpvar = 0 THEN RUN spara_UI (INPUT skarpvar).
      RUN avb_UI.  
      
      
      {musarrow.i}
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OVER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OVER WINDOW-3
ON CHOOSE OF BTN_OVER IN FRAME FRAME-C
DO:
   antal_valda = BRW_K4:NUM-SELECTED-ROWS NO-ERROR.
   IF antal_valda = 0 THEN DO:
      MESSAGE "Du måste välja något momskonto." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   antal_valda = BRW_KOSTMOMS:NUM-SELECTED-ROWS NO-ERROR.
   antal_raknare = 1.   
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = BRW_KOSTMOMS:FETCH-SELECTED-ROW(antal_raknare). 
      RUN setlastrowid_UI IN brwproc[8] (INPUT ROWID(kosttemp)).
      ASSIGN
      kosttemp.MOMSEXTERNT = momstemp.MOMSEXTERN 
      kosttemp.MOMSID = momstemp.MOMSID
      kosttemp.MOMSKOD = momstemp.MOMSKOD.                
      antal_raknare = antal_raknare + 1.   
   END.         
   RUN refreshbrw_UI IN brwproc[8].
   RUN lastselectdyn_UI IN brwproc[8]. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SNABB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SNABB WINDOW-3
ON CHOOSE OF BTN_SNABB IN FRAME FRAME-C /* Snabbspara */
DO:
   {muswait.i}
   IF skarpvar = 0 THEN RUN spara_UI (INPUT skarpvar).
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_MED
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_MED WINDOW-3
ON CHOOSE OF FBTN_MED IN FRAME FRAME-C /* Text till faktura */
DO:
   {AVBGOM.I}      
   MESSAGE "UNDER UTVECKLING" VIEW-AS ALERT-BOX.
   {AVBFRAM.I}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_MOMS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_MOMS WINDOW-3
ON CHOOSE OF FBTN_MOMS IN FRAME FRAME-C /* Kontering */
DO:    
   /*
   IF TOG_MOMS = TRUE THEN DO:
      RETURN NO-APPLY.
   END.
   */
   FIND FIRST gfaktemp WHERE gfaktemp.ORDNING = 3 NO-ERROR.
   IF gfaktemp.TOTALT < 0 THEN DO:
      MESSAGE "Du har ett negativt totalbelopp på denna faktura. Gör en debetfaktura i stället."
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GRAN"  OR Guru.Konstanter:globforetag = "GKAL" THEN DO:   
      IF vartyp = 5 THEN DO:
         FOR EACH faktfriatemp WHERE faktfriatemp.FAKTNR = vfaktplantemp.FAKTNR AND
         faktfriatemp.FDELNR = efaktkredtemp.FDELNR AND 
         faktfriatemp.FAKTTEXT = "Korigering mot takpris" NO-LOCK:
            RUN fribortk_UI IN faktupphmth (INPUT faktfriatemp.ROWFRI).
            DELETE faktfriatemp. 
         END.
         
      END.
   END.   
   RUN moms_UI.   
   IF musz = TRUE THEN DO:   
      musz = FALSE.           
      RUN momsbort_UI (FALSE).         
      RUN moms_UI.      
   END.   
   TOG_MOMS = TRUE.
   ASSIGN
   FBTN_MOMS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   FBTN_MOMSBORT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
   TOG_MOMS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      MESSAGE "Kontering klar!" VIEW-AS ALERT-BOX.
   END.
   {musarrow.i}
   /*
   DISPLAY TOG_MOMS WITH FRAME {&FRAME-NAME}.
   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_MOMSBORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_MOMSBORT WINDOW-3
ON CHOOSE OF FBTN_MOMSBORT IN FRAME FRAME-C /* Ta bort kontering */
DO:    
   IF TOG_MOMS = FALSE THEN DO:
      RETURN NO-APPLY.
   END.
   MESSAGE "Vill du ta bort kontoberäkningarna och göra om dem från början ?" 
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Fakturera"
   UPDATE answer1 AS LOGICAL.
   {muswait.i}
   IF answer1 THEN DO:
      TOG_MOMS = FALSE.
      RUN momsbort_UI (TRUE).
      /*
      DISPLAY TOG_MOMS WITH FRAME {&FRAME-NAME}.
      */
      TOG_MOMS:HIDDEN = TRUE.      
      IF TOG_MOMS = FALSE THEN DO:
         IF RAD_VISA = 2 AND TOG_DIFF = TRUE THEN DO: 
            ASSIGN      
            BTN_BACK:HIDDEN = FALSE
            BTN_OVER:HIDDEN = FALSE.
         END.
         IF valaconttak = TRUE THEN DO:
            IF vartyp = 5 THEN DO:
               
            END.
            IF vartyp = 3 THEN DO:
               
            END.
            
         END.
      END.
      TOG_MOMS = FALSE.
      ASSIGN
      FBTN_MOMS:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FBTN_MOMSBORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_MOMS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      /*
      DISPLAY TOG_MOMS WITH FRAME {&FRAME-NAME}.
      */
   END.
   ELSE DO:
      TOG_MOMS = TRUE.
      ASSIGN
      FBTN_MOMS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
      FBTN_MOMSBORT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      TOG_MOMS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      /*
      DISPLAY TOG_MOMS WITH FRAME {&FRAME-NAME}.      
      */
      IF Guru.Konstanter:faktsekvar[5] = TRUE THEN DO:
         ASSIGN
         FBTN_MOMSBORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         FBTN_MOMS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         TOG_MOMS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
       END.
   END.   
   APPLY "VALUE-CHANGED" TO RAD_VISA IN FRAME {&FRAME-NAME}.
   {musarrow.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_OK WINDOW-3
ON CHOOSE OF FBTN_OK IN FRAME FRAME-C /* Fakturera */
DO:
   {AMERICANEUROPEAN.I}
   RUN klar_UI.
   {EUROPEANAMERICAN.I}
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_VISFAK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VISFAK WINDOW-3
ON CHOOSE OF FBTN_VISFAK IN FRAME FRAME-C /* Visa faktura */
DO:   
    
   skrivut = FALSE.
   IF faktskap = FALSE THEN DO:              
      RUN spara_UI (INPUT skarpvar).
   END.
   {AVBGOM.I}
   {AMERICANEUROPEAN.I}
   RUN VLOPFAKKA.W (INPUT infakplannr,INPUT kreditnrvar,INPUT TRUE,INPUT TABLE efaktkredtemp,INPUT TABLE sumtidtemp).
   {EUROPEANAMERICAN.I}
   {AVBFRAM.I}
   {musarrow.i}
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-TOMDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TOMDAT WINDOW-3
ON LEAVE OF FILL-IN-TOMDAT IN FRAME FRAME-C
DO:
   ASSIGN
   FILL-IN-TOMDAT = INPUT FILL-IN-TOMDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Ta_ej_med_markerade_posterKOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Ta_ej_med_markerade_posterKOST WINDOW-3
ON CHOOSE OF MENU-ITEM m_Ta_ej_med_markerade_posterKOST /* Ta ej med markerade poster */
DO:
   IF TOG_MOMS = TRUE THEN DO:
      RUN kontokoll_UI.
   END.
   ELSE DO:
      IF vartyp = 1 OR vartyp = 2 OR vartyp = 52 THEN RETURN NO-APPLY.
      ELSE IF vartyp = 3 THEN RETURN NO-APPLY.
      antal_valda = BRW_KOST:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} NO-ERROR.
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:                                      
         status-ok = BRW_KOST:FETCH-SELECTED-ROW(antal_raknare).   
         IF kosttemp.MED = FALSE OR kosttemp.MED = ? THEN. 
         ELSE RUN medkost_UI (INPUT 3).
         antal_raknare = antal_raknare + 1.   
      END.
      
      RUN setselectvalue_UI IN brwproc[7] (INPUT "MED",INPUT "FALSE").
      /*
      DO WHILE antal_raknare LE antal_valda:                                      
         status-ok = BRW_KOST:FETCH-SELECTED-ROW(antal_raknare).   
         IF kosttemp.MED = FALSE OR kosttemp.MED = ? THEN RUN medkost_UI (INPUT 4).
         RUN valrattright_UI IN brwproc[7] (INPUT antal_raknare,INPUT TRUE,INPUT ROWID(kosttemp)).
         antal_raknare = antal_raknare + 1.   
      END.
      
      RUN setselectvalue_UI IN brwproc[7] (INPUT "MED",INPUT "FALSE").     
      antal_valda = BRW_KOST:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:                   
         status-ok = BRW_KOST:FETCH-SELECTED-ROW(antal_raknare).   
         RUN medkost_UI (INPUT 3).
         RUN valrattright_UI IN brwproc[7] (INPUT antal_raknare,INPUT TRUE,INPUT ROWID(kosttemp)).
         antal_raknare = antal_raknare + 1.   
      END.
      */
      RUN refreshbrw_UI IN brwproc[7].
      RUN lastselectdyn_UI IN brwproc[7].
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Ta_ej_med_markerade_posterTID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Ta_ej_med_markerade_posterTID WINDOW-3
ON CHOOSE OF MENU-ITEM m_Ta_ej_med_markerade_posterTID /* Ta ej med markerade poster */
DO:
   IF TOG_MOMS = TRUE THEN DO:
      RUN kontokoll_UI.
   END.
   ELSE DO:               
      IF vartyp = 1 OR vartyp = 2 OR vartyp = 52 THEN RETURN NO-APPLY.
      ELSE IF vartyp = 3 THEN RETURN NO-APPLY.
      antal_valda = BRW_TID:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} NO-ERROR.
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:                                      
         status-ok = BRW_TID:FETCH-SELECTED-ROW(antal_raknare).   
         IF sumtidtemp.MED = FALSE OR sumtidtemp.MED = ? THEN. 
         ELSE  RUN bertidkost_UI (INPUT FALSE).
            
         antal_raknare = antal_raknare + 1.   
      END.  
      RUN setselectvalue_UI IN brwproc[9] (INPUT "MED",INPUT "FALSE").   
      /*
      DO WHILE antal_raknare LE antal_valda:                                      
         status-ok = BRW_TID:FETCH-SELECTED-ROW(antal_raknare).   
         IF sumtidtemp.MED = FALSE OR sumtidtemp.MED = ? THEN RUN bertidkost_UI (INPUT TRUE).
         RUN valrattright_UI IN brwproc[9] (INPUT antal_raknare,INPUT TRUE,INPUT ROWID(sumtidtemp)).
         antal_raknare = antal_raknare + 1.   
      END.
      RUN setselectvalue_UI IN brwproc[9] (INPUT "MED",INPUT "FALSE").
      antal_valda = BRW_TID:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}.
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:                                      
         status-ok = BRW_TID:FETCH-SELECTED-ROW(antal_raknare).   
         RUN bertidkost_UI (INPUT FALSE).
         RUN valrattright_UI IN brwproc[9] (INPUT antal_raknare,INPUT TRUE,INPUT ROWID(sumtidtemp)).
         antal_raknare = antal_raknare + 1.   
      END.
      */
      
      
      RUN refreshbrw_UI IN brwproc[9].
      RUN lastselectdyn_UI IN brwproc[9].
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Ta_med_markerade_posterKOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Ta_med_markerade_posterKOST WINDOW-3
ON CHOOSE OF MENU-ITEM m_Ta_med_markerade_posterKOST /* Ta med markerade poster */
DO:
   IF TOG_MOMS = TRUE THEN DO:
      RUN kontokoll_UI.
   END.
   ELSE DO:
      IF vartyp = 1 OR vartyp = 2 OR vartyp = 52 THEN RETURN NO-APPLY.
      ELSE IF vartyp = 3 THEN RETURN NO-APPLY.
      antal_valda = BRW_KOST:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} NO-ERROR.
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:                                      
         status-ok = BRW_KOST:FETCH-SELECTED-ROW(antal_raknare).          
         IF kosttemp.MED = TRUE THEN.
         ELSE  RUN medkost_UI (INPUT 3).
         antal_raknare = antal_raknare + 1.   
      END.
      RUN setselectvalue_UI IN brwproc[7] (INPUT "MED",INPUT "TRUE").
      /*
      DO WHILE antal_raknare LE antal_valda:                                      
         status-ok = BRW_KOST:FETCH-SELECTED-ROW(antal_raknare).          
         IF kosttemp.MED = TRUE THEN RUN medkost_UI (INPUT 4).
         RUN valrattright_UI IN brwproc[7] (INPUT antal_raknare,INPUT TRUE,INPUT ROWID(kosttemp)).
         antal_raknare = antal_raknare + 1.   
      END.
      RUN setselectvalue_UI IN brwproc[7] (INPUT "MED",INPUT "TRUE").
      antal_valda = BRW_KOST:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} NO-ERROR.
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:                                      
         status-ok = BRW_KOST:FETCH-SELECTED-ROW(antal_raknare).   
         RUN medkost_UI (INPUT 3).
         RUN valrattright_UI IN brwproc[7] (INPUT antal_raknare,INPUT TRUE,INPUT ROWID(kosttemp)).
         antal_raknare = antal_raknare + 1.   
      END.
      */
      RUN refreshbrw_UI IN brwproc[7].
      RUN lastselectdyn_UI IN brwproc[7].
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Ta_med_markerade_posterTID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Ta_med_markerade_posterTID WINDOW-3
ON CHOOSE OF MENU-ITEM m_Ta_med_markerade_posterTID /* Ta med markerade poster */
DO:
   IF TOG_MOMS = TRUE THEN DO:
      RUN kontokoll_UI.
   END.
   ELSE DO:
      IF vartyp = 1 OR vartyp = 2 OR vartyp = 52 THEN RETURN NO-APPLY.
      ELSE IF vartyp = 3 THEN RETURN NO-APPLY.
      antal_valda = BRW_TID:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} NO-ERROR.
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:                                      
         status-ok = BRW_TID:FETCH-SELECTED-ROW(antal_raknare).   
         IF sumtidtemp.MED = TRUE THEN.
         ELSE RUN bertidkost_UI (INPUT TRUE).
        
         antal_raknare = antal_raknare + 1.   
      END.     
      RUN setselectvalue_UI IN brwproc[9] (INPUT "MED",INPUT "TRUE").
      /*
      DO WHILE antal_raknare LE antal_valda:                                      
         status-ok = BRW_TID:FETCH-SELECTED-ROW(antal_raknare).   
         IF sumtidtemp.MED = TRUE THEN RUN bertidkost_UI (INPUT FALSE).
         RUN valrattright_UI IN brwproc[9] (INPUT antal_raknare,INPUT TRUE,INPUT ROWID(sumtidtemp)).
         antal_raknare = antal_raknare + 1.   
      END.
      RUN setselectvalue_UI IN brwproc[9] (INPUT "MED",INPUT "TRUE").
      antal_valda = BRW_TID:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} NO-ERROR.
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:                                      
         status-ok = BRW_TID:FETCH-SELECTED-ROW(antal_raknare).   
         RUN bertidkost_UI (INPUT TRUE).
         RUN valrattright_UI IN brwproc[9] (INPUT antal_raknare,INPUT TRUE,INPUT ROWID(sumtidtemp)).
         antal_raknare = antal_raknare + 1.   
      END.
      */
      RUN refreshbrw_UI IN brwproc[9].
      RUN lastselectdyn_UI IN brwproc[9].
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VISA WINDOW-3
ON VALUE-CHANGED OF RAD_VISA IN FRAME FRAME-C
DO:   
   {muswait.i}
   RAD_VISA = INPUT RAD_VISA.         
   RUN rad_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_DIFF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_DIFF WINDOW-3
ON VALUE-CHANGED OF TOG_DIFF IN FRAME FRAME-C /* Differentierad moms */
DO:
   TOG_DIFF = INPUT TOG_DIFF.   
   RUN togdiff_UI.      
   {musarrow.i}
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
   {muswait.i}
   IF answer1 THEN DO:
      TOG_MOMS = FALSE.
      RUN momsbort_UI (TRUE).
      /*
      DISPLAY TOG_MOMS WITH FRAME {&FRAME-NAME}.
      */
      TOG_MOMS:HIDDEN = TRUE.      
      IF TOG_MOMS = FALSE THEN DO:
         IF RAD_VISA = 2 AND TOG_DIFF = TRUE THEN DO: 
            ASSIGN      
            BTN_BACK:HIDDEN = FALSE
            BTN_OVER:HIDDEN = FALSE.
         END.
         IF valaconttak = TRUE THEN DO:
            IF vartyp = 5 THEN DO:
               
            END.
            IF vartyp = 3 THEN DO:
               
            END.
            
         END.
      END.
      ASSIGN
      FBTN_MOMS:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FBTN_MOMSBORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_MOMS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   ELSE DO:
      TOG_MOMS = TRUE.
      ASSIGN
      FBTN_MOMS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
      FBTN_MOMSBORT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      TOG_MOMS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      /*
      DISPLAY TOG_MOMS WITH FRAME {&FRAME-NAME}.      
      */
      IF Guru.Konstanter:faktsekvar[5] = TRUE THEN DO:
         ASSIGN
         FBTN_MOMSBORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         FBTN_MOMS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         TOG_MOMS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
       END.
   END.   
   APPLY "VALUE-CHANGED" TO RAD_VISA IN FRAME {&FRAME-NAME}.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_ALLT
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
   {WIN_M_START.I}


   {ALLSTARTDYN.I}
         /*
   "Fastpris" "Avtal" "A-contofakt." "Löpande räkning"  "Takprisfakt." "Fri fakturering"  
   faktyptemp.VIFAKTTYP = "Fastpris" faktyptemp.TYP       = 1
   faktyptemp.VIFAKTTYP = "Avtal" faktyptemp.TYP       = 2
   faktyptemp.VIFAKTTYP = "A-contofakt." faktyptemp.TYP       = 3
   faktyptemp.VIFAKTTYP = "Löpande räkning" faktyptemp.TYP       = 4
   faktyptemp.VIFAKTTYP = "Takprisfakt." faktyptemp.TYP       = 5
   faktyptemp.VIFAKTTYP = "Fri fakturering" faktyptemp.TYP       = 6
   faktyptemp.VIFAKTTYP = "Bokföringsfakt." faktyptemp.TYP       = 7
   faktyptemp.VIFAKTTYP = "Löpande utan" faktyptemp.TYP       = 8
   */    

   {muswait.i}  
   FIND FIRST efaktkredtemp NO-LOCK NO-ERROR.
   ASSIGN
   infakplannr = efaktkredtemp.FAKTNR
   kreditnrvar = efaktkredtemp.FDELNR 
   FILL-IN_FDELNR = efaktkredtemp.FDELNR.
   RUN huvud2_UI.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      LEAVE MAIN-BLOCK.
   END.
   {FRMSIZE.I}
   FBTN_MED:HIDDEN = TRUE.
   APPLY "VALUE-CHANGED" TO RAD_VISA.
   IF Guru.Konstanter:faktsekvar[5] = TRUE THEN DO:
      ASSIGN
      FBTN_MOMSBORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      FBTN_MOMS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_MOMS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   {musarrow.i}   
    {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN 
   WAIT-FOR CLOSE OF THIS-PROCEDURE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-3 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_FRI:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[2] 
      (INPUT BRW_PLAN:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[3] 
      (INPUT BRW_UPP:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[4] 
      (INPUT BRW_K4:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[5] 
      (INPUT BRW_ALLT:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[6] 
      (INPUT BRW_GKOST:HANDLE IN FRAME {&FRAME-NAME}).
   RUN brwsetupstop_UI IN brwproc[6] (INPUT 1).
   RUN DYNBRW.P PERSISTENT SET brwproc[7] 
      (INPUT BRW_KOST:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[8] 
      (INPUT BRW_KOSTMOMS:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[9] 
      (INPUT BRW_TID:HANDLE IN FRAME {&FRAME-NAME}).
   RUN setcolindex_UI IN brwproc[1] (INPUT "SUBSTRING(TYP,1,4) BY AONR BY DELNR BY FAKTTEXT").
   RUN setcolindex_UI IN brwproc[3] (INPUT "ORDNING").
   RUN setshiftcol_UI IN brwproc[9] (INPUT TRUE).
   IF Guru.Konstanter:appcon THEN DO:
      RUN FAKTUPPHMT.P PERSISTENT SET faktupphmth ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN FAKTUPPHMT.P PERSISTENT SET faktupphmth.
   END.
   /*laddar konton*/
   IF Guru.Konstanter:appcon THEN DO:
      RUN FAKKAPP.P PERSISTENT SET fakkoproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.         
   END.
   ELSE DO:
      RUN FAKKAPP.P PERSISTENT SET fakkoproch.         
   END.
   RUN laddakontotemp_UI IN fakkoproch 
   (OUTPUT TABLE kundfodrantemp, OUTPUT TABLE intakttemp, OUTPUT TABLE motparttemp, OUTPUT TABLE momstemp).   
   RUN laddakfmtemp_UI IN fakkoproch 
   (OUTPUT TABLE styrkfmtemp,OUTPUT TABLE faktureringstyptemp, OUTPUT TABLE kundtyptemp).   
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
   {muswait.i}
   {musarrow.i}
   IF VALID-HANDLE(faktupphmth) THEN DELETE PROCEDURE faktupphmth.      
   IF VALID-HANDLE(fakkoproch) THEN DELETE PROCEDURE fakkoproch. 
   {BORTBRWPROC.I}
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE back_UI WINDOW-3 
PROCEDURE back_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   antal_valda = BRW_KOSTMOMS:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME} NO-ERROR.
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                      
      status-ok = BRW_KOSTMOMS:FETCH-SELECTED-ROW(antal_raknare).                                  
      ASSIGN
      kosttemp.MOMSEXTERNT = 0 
      kosttemp.MOMSID = 0
      kosttemp.MOMSKOD = "".
      RUN valrattright_UI IN brwproc[8] (INPUT antal_raknare,INPUT TRUE,INPUT ROWID(kosttemp)).
      antal_raknare = antal_raknare + 1.   
   END.
   RUN refreshbrw_UI IN brwproc[8].
   RUN lastselectdyn_UI IN brwproc[8].
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
   /*RÄTT TEXTER TILL BEFATTNING*/
   IF Guru.Konstanter:appcon THEN DO:
      RUN FABEFAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT-OUTPUT TABLE sumtidtemp).
   END.
   ELSE DO:
      RUN FABEFAPP.P 
      (INPUT-OUTPUT TABLE sumtidtemp).
   END.        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE beloppleav_UI WINDOW-3 
PROCEDURE beloppleav_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF faktstarttemp.BELOPP NE INPUT BROWSE BRW_PLAN faktstarttemp.BELOPP THEN DO:
      IF TOG_MOMS = TRUE THEN DO:
         RUN kontokoll_UI.
      END.
      ELSE IF faktstarttemp.VFAKTNR NE 0 THEN DO:
         MESSAGE "Det går inte att ändra på denna post då den redan är fakturerad."   
         VIEW-AS ALERT-BOX.
      END.
      ELSE DO:
         IF vartyp = 3 THEN DO:
            IF faktstarttemp.BELOPP = INPUT BROWSE BRW_PLAN faktstarttemp.BELOPP THEN musz = musz.
            ELSE DO:
               IF TOG_MOMS = TRUE THEN DO:
                  RUN kontokoll_UI.
               END.
               ELSE DO:
                  IF faktstarttemp.FAKTURERAD = TRUE THEN DO:            
                     DO TRANSACTION:
                        faktstarttemp.BELOPP = INPUT BROWSE BRW_PLAN faktstarttemp.BELOPP.   
                        DISPLAY faktstarttemp.BELOPP WITH BROWSE BRW_PLAN.      
                        FIND FIRST gfaktemp WHERE gfaktemp.ORDNING = 3 NO-ERROR. 
                        gfaktemp.TOTALT = gfaktemp.OVRIG + faktstarttemp.BELOPP. 
                        IF rundavar = TRUE THEN DO:
                           ASSIGN
                           gfaktemp.TOTALT = runda(gfaktemp.TOTALT).      
                        END.
                     END.                 
                  END.
               END.   
               DISPLAY faktstarttemp.BELOPP WITH BROWSE BRW_PLAN.
            END.
         END.   
         ELSE DO:
            MESSAGE "Det går inte att ändra på denna post."   
            VIEW-AS ALERT-BOX.
         END.
      END.
      DISPLAY faktstarttemp.BELOPP WITH BROWSE BRW_PLAN.
      RUN refreshbrw_UI IN brwproc[2].
      RUN refreshbrw_UI IN brwproc[6].
   END.

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
   IF rundavar = TRUE THEN DO:
      ASSIGN
      gfaktemp.TOTALT = runda(gfaktemp.TOTALT)
      gfaktemp.RES = runda(gfaktemp.RES) 
      gfaktemp.ARBKOST = runda(gfaktemp.ARBKOST) 
      gfaktemp.LONTILL = runda(gfaktemp.LONTILL) 
      gfaktemp.MTRL = runda(gfaktemp.MTRL) 
      gfaktemp.OBELOPP = runda(gfaktemp.OBELOPP) 
      gfaktemp.OVRIG = runda(gfaktemp.OVRIG) 
      gfaktemp.TRAKT = runda(gfaktemp.TRAKT)       
      gfaktemp.KBELOPP = runda(gfaktemp.KBELOPP).   
   END. 
   vararbkost = gfaktemp.ARBKOST.   
   vartrakost = gfaktemp.TRAKT.
   IF vartyp = 3 OR vartyp = 1 OR vartyp = 2 OR vartyp = 52 THEN DO:
      gfaktemp.TOTALT = gfaktemp.TOTALT + 
                        gfaktemp.MTRL + gfaktemp.OVRIG + gfaktemp.ARBKOST + 
                        gfaktemp.OBELOPP + gfaktemp.TRAKT + gfaktemp.RES + 
                        gfaktemp.LONTILL + gfaktemp.KBELOPP.
   END.
   ELSE DO:
      gfaktemp.TOTALT = gfaktemp.MTRL + gfaktemp.OVRIG + gfaktemp.ARBKOST + 
                        gfaktemp.OBELOPP + gfaktemp.TRAKT + gfaktemp.RES + 
                        gfaktemp.LONTILL + gfaktemp.KBELOPP.      
   END.       
   
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
   {muswait.i}
   FIND FIRST gfaktemp WHERE gfaktemp.ORDNING = 3 NO-ERROR.
   IF medvar = ? OR medvar = FALSE THEN DO:    
      IF kundregeltemp.TIMRGL = "TIDREDOVISNING" THEN DO:
         gfaktemp.ARBKOST = gfaktemp.ARBKOST - sumtidtemp.BELOPP.                    
      END.       
      IF kundregeltemp.OVERTIDRGL = "TIDREDOVISNING" THEN DO:
         gfaktemp.OBELOPP = gfaktemp.OBELOPP - sumtidtemp.OBELOPP.                               
      END. 
      IF kundregeltemp.TIMRGL = "KUNDENS EGNA" THEN DO:
         gfaktemp.ARBKOST = gfaktemp.ARBKOST - sumtidtemp.BELOPP.                    
      END.  
      IF kundregeltemp.OVERTIDRGL = "EGNA REGLER" THEN DO:
         gfaktemp.OBELOPP = gfaktemp.OBELOPP - sumtidtemp.OBELOPP.                               
      END.         
      IF kundregeltemp.TRAKTRGL = "TIDREDOVISNING" OR kundregeltemp.TRAKTRGL = "EGET PRIS" THEN DO:         
         gfaktemp.TRAKT = gfaktemp.TRAKT - sumtidtemp.TBELOPP.                            
      END.
      IF kundregeltemp.LONRGL = "TIDREDOVISNING" OR kundregeltemp.LONRGL = "ENDAST MILERSÄTTNING" THEN DO:
         gfaktemp.LONTILL = gfaktemp.LONTILL - sumtidtemp.LONKOST.                     
      END.          
      gfaktemp.RES = gfaktemp.RES - sumtidtemp.RESKOSTDEC.    
   END.       
   ELSE DO:
      IF kundregeltemp.TIMRGL = "TIDREDOVISNING" THEN DO:
         gfaktemp.ARBKOST = gfaktemp.ARBKOST + sumtidtemp.BELOPP.                    
      END.       
      IF kundregeltemp.OVERTIDRGL = "TIDREDOVISNING" THEN DO:
         gfaktemp.OBELOPP = gfaktemp.OBELOPP + sumtidtemp.OBELOPP.                               
      END. 
      IF kundregeltemp.TIMRGL = "KUNDENS EGNA" THEN DO:
         gfaktemp.ARBKOST = gfaktemp.ARBKOST + sumtidtemp.BELOPP.                    
      END.  
      IF kundregeltemp.OVERTIDRGL = "EGNA REGLER" THEN DO:
         gfaktemp.OBELOPP = gfaktemp.OBELOPP + sumtidtemp.OBELOPP.                               
      END.         
      IF kundregeltemp.TRAKTRGL = "TIDREDOVISNING" OR kundregeltemp.TRAKTRGL = "EGET PRIS" THEN DO:         
         gfaktemp.TRAKT = gfaktemp.TRAKT + sumtidtemp.TBELOPP.                            
      END.
      IF kundregeltemp.LONRGL = "TIDREDOVISNING" OR kundregeltemp.LONRGL = "ENDAST MILERSÄTTNING" THEN DO:
         gfaktemp.LONTILL = gfaktemp.LONTILL + sumtidtemp.LONKOST.                     
      END.          
      gfaktemp.RES = gfaktemp.RES + sumtidtemp.RESKOSTDEC.    
   END. 
   
   IF rundavar = TRUE THEN DO:
      ASSIGN
      gfaktemp.TOTALT = runda(gfaktemp.TOTALT)
      gfaktemp.RES = runda(gfaktemp.RES) 
      gfaktemp.ARBKOST = runda(gfaktemp.ARBKOST) 
      gfaktemp.LONTILL = runda(gfaktemp.LONTILL) 
      gfaktemp.MTRL = runda(gfaktemp.MTRL) 
      gfaktemp.OBELOPP = runda(gfaktemp.OBELOPP) 
      gfaktemp.OVRIG = runda(gfaktemp.OVRIG) 
      gfaktemp.TRAKT = runda(gfaktemp.TRAKT)       
      gfaktemp.KBELOPP = runda(gfaktemp.KBELOPP).
   END.                               
   IF vartyp = 3 OR vartyp = 1 OR vartyp = 2  OR vartyp = 52 THEN DO:
      gfaktemp.TOTALT = gfaktemp.TOTALT + 
                        gfaktemp.MTRL + gfaktemp.OVRIG + gfaktemp.ARBKOST + 
                        gfaktemp.OBELOPP + gfaktemp.TRAKT + gfaktemp.RES + 
                        gfaktemp.LONTILL + gfaktemp.KBELOPP.
   END.
   ELSE DO:
      gfaktemp.TOTALT = gfaktemp.MTRL + gfaktemp.OVRIG + gfaktemp.ARBKOST + 
                        gfaktemp.OBELOPP + gfaktemp.TRAKT + gfaktemp.RES + 
                        gfaktemp.LONTILL + gfaktemp.KBELOPP.
   END. 
   RUN refreshbrw_UI IN brwproc[6].
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE brwfri_UI WINDOW-3 
PROCEDURE brwfri_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   DEFINE INPUT PARAMETER nyvar AS LOGICAL NO-UNDO.
   DEFINE VARIABLE knappvar AS INTEGER NO-UNDO.
   IF TOG_MOMS = TRUE THEN DO:
      RUN kontokoll_UI.
   END.
   ELSE DO:
      status-ok = BRW_FRI:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      /*
      IF NOT AVAILABLE faktfriatemp THEN DO:
         RETURN NO-APPLY.         
      END.
      */
      EMPTY TEMP-TABLE efaktfriatemp NO-ERROR. 
      IF nyvar = TRUE THEN DO:
         FIND LAST faktfriatemp USE-INDEX LOPNR NO-LOCK NO-ERROR.
         nya = TRUE.
         CREATE efaktfriatemp. 
         ASSIGN    
         efaktfriatemp.LOPNR = 1
         efaktfriatemp.ROWFRI = ?
         efaktfriatemp.FAKTNR = vfaktplantemp.FAKTNR
         efaktfriatemp.FDELNR = FILL-IN_FDELNR
         efaktfriatemp.FAKTURERAD = TRUE.        
         IF AVAILABLE faktfriatemp THEN efaktfriatemp.LOPNR = faktfriatemp.LOPNR + 1.
         frirow = ?.
      END.
      ELSE DO:
         IF faktfriatemp.FAKTURERAD = TRUE THEN DO:
            FIND FIRST gfaktemp WHERE gfaktemp.ORDNING = 3 NO-ERROR.
            ASSIGN         
            gfaktemp.TOTALT = gfaktemp.TOTALT - faktfriatemp.TOTALT.
            RUN gladda_UI (INPUT 2).
            
         END.
         CREATE efaktfriatemp.
         BUFFER-COPY faktfriatemp TO efaktfriatemp.
         frirow = faktfriatemp.ROWFRI.
      END.
      
      RUN FRIANYFA.W (INPUT TRUE,INPUT infakplannr,INPUT FILL-IN_FDELNR,INPUT rundavar,efaktkredtemp.FAKTTYPID,INPUT-OUTPUT frirow,INPUT-OUTPUT nya,OUTPUT knappvar,INPUT TABLE faktaonrtemp,INPUT TABLE efaktfriatemp).
      
      IF knappvar = 1 THEN DO:
         
      END.
      ELSE IF knappvar = 4 THEN DO:
      /*OK*/                  
         
      END.
      RUN hamtfrik_UI IN faktupphmth (INPUT infakplannr,INPUT FILL-IN_FDELNR,OUTPUT TABLE faktfriatemp).
      FIND FIRST faktfriatemp WHERE faktfriatemp.FAKTTEXT = "Dubbel-klicka på denna rad för nyupplägg" NO-LOCK NO-ERROR.
      IF AVAILABLE faktfriatemp THEN DELETE faktfriatemp.
      /*
      IF NOT AVAILABLE faktfriatemp THEN DO: 
         CREATE faktfriatemp.   
         ASSIGN                                    
         faktfriatemp.FAKTNR = infakplannr 
         faktfriatemp.FDELNR = FILL-IN_FDELNR
         faktfriatemp.FAKTURERAD = FALSE
         faktfriatemp.FAKTTEXT = "Dubbel-klicka på denna rad för nyupplägg".                               
      END. 
      */
      FIND FIRST faktfriatemp WHERE faktfriatemp.ROWFRI = frirow NO-LOCK NO-ERROR.
      IF NOT AVAILABLE faktfriatemp THEN DO:
         FIND FIRST faktfriatemp NO-LOCK NO-ERROR.
      END.
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(faktfriatemp)).              
      RUN openbdynspec_UI IN brwproc[1].
      RUN lastselectdyn_UI IN brwproc[1].      
   END.
   RUN refreshbrw_UI IN brwproc[6].
   ASSIGN
   musz = FALSE      
   nya = FALSE.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE brwtid_UI WINDOW-3 
PROCEDURE brwtid_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   IF TOG_MOMS = TRUE THEN DO:
      RUN kontokoll_UI.
   END.
   ELSE DO:
      IF vartyp = 1 OR vartyp = 2 OR vartyp = 52 THEN musz = musz.
      ELSE IF vartyp = 3 THEN musz = musz.
      ELSE DO:         
         {muswait.i}
         status-ok = BRW_TID:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.                
      END.   
      FIND FIRST gfaktemp WHERE gfaktemp.ORDNING = 3 NO-ERROR.
      
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
  DISPLAY FILL-IN_FDELNR TOG_DIFF TOG_MOMS 
      WITH FRAME FRAME-C IN WINDOW WINDOW-3.
  IF AVAILABLE vfaktplantemp THEN 
    DISPLAY vfaktplantemp.FAKTNR vfaktplantemp.NAMN 
      WITH FRAME FRAME-C IN WINDOW WINDOW-3.
  ENABLE TOG_DIFF FBTN_OK FBTN_VISFAK FBTN_MOMSBORT FBTN_MOMS FBTN_MED 
         BTN_SNABB BTN_OK BTN_FRINY BTN_FRIAND BTN_FRIBORT BTN_AVB 
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
   ASSIGN
   faktstarttemp.FDELNR = FILL-IN_FDELNR
   faktstarttemp.FAKTURADATUM = TODAY
   faktstarttemp.FAKTURERAD = TRUE. 
   gfaktemp.TOTALT = gfaktemp.TOTALT + faktstarttemp.BELOPP.
   IF rundavar = TRUE THEN DO:
      ASSIGN
      gfaktemp.TOTALT = runda(gfaktemp.TOTALT).      
   END.
   
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
   {muswait.i}      
   FIND FIRST gfaktemp WHERE gfaktemp.ORDNING = 3 NO-ERROR.
   IF NOT AVAILABLE gfaktemp THEN CREATE gfaktemp.
   ASSIGN
   gfaktemp.TYPTEXT = "Denna faktura" 
   gfaktemp.TOTALT = 0
   gfaktemp.ORDNING = 3.
   IF vartyp = 52 THEN DO:
      OPEN QUERY BRW_UPP FOR EACH faktupparbtemp WHERE 
      faktupparbtemp.FAKTNR = infakplannr NO-LOCK BY faktupparbtemp.AONR BY
      faktupparbtemp.DELNR BY faktupparbtemp.UPLAN% /*BY faktupparbtemp.FAKTURADATUM BY faktupparbtemp.KRITERIUM*/ /*DESCENDING */.
      GET FIRST BRW_UPP NO-LOCK.
      DO WHILE AVAILABLE(faktupparbtemp):
         IF faktupparbtemp.VKREDIT = 0 AND faktupparbtemp.FAKTURERAD = TRUE THEN DO:
            gfaktemp.TOTALT = gfaktemp.TOTALT + faktupparbtemp.FAKTBELOPP.
         END.
         GET NEXT BRW_UPP NO-LOCK.
      END. 
      IF rundavar = TRUE THEN DO:
         ASSIGN
         gfaktemp.TOTALT = runda(gfaktemp.TOTALT).
      END.
      GET FIRST BRW_UPP NO-LOCK.   
      ENABLE BRW_UPP WITH FRAME {&FRAME-NAME}.   
      RUN open_UI.               
      RAD_VISA = 6.         
      RUN visa_UI (INPUT TRUE).
   END.
   ELSE DO:
      IF vartyp = 3 THEN DO:   
         OPEN QUERY BRW_PLAN FOR EACH faktstarttemp WHERE 
         faktstarttemp.FAKTNR = infakplannr AND faktstarttemp.START = ""
         NO-LOCK.            
      END.
      ELSE DO:
         OPEN QUERY BRW_PLAN FOR EACH faktstarttemp WHERE 
         faktstarttemp.FAKTNR = infakplannr AND faktstarttemp.BELOPP NE 0 NO-LOCK.            
      END.   
      GET FIRST BRW_PLAN NO-LOCK.
      DO WHILE AVAILABLE(faktstarttemp):
         IF faktstarttemp.VKREDIT = 0 AND faktstarttemp.FAKTURERAD = TRUE THEN DO:
            gfaktemp.TOTALT = gfaktemp.TOTALT + faktstarttemp.BELOPP.
         END.
         GET NEXT BRW_PLAN NO-LOCK.
      END. 
      IF vartyp = 3 THEN DO:
         FIND FIRST faktstarttemp WHERE faktstarttemp.FAKTNR = infakplannr AND
         faktstarttemp.VKREDIT = 0 AND faktstarttemp.FAKTURERAD = TRUE AND 
         faktstarttemp.START = "" NO-LOCK NO-ERROR.            
         IF NOT AVAILABLE faktstarttemp THEN DO:
            CREATE faktstarttemp.      
            ASSIGN
            faktstarttemp.FAKTNR = infakplannr
            faktstarttemp.PLANDATUM = TODAY
            faktstarttemp.FAKTURADATUM = TODAY
            faktstarttemp.FAKTURERAD = TRUE
            faktstarttemp.ORDNING = 2.       
            OPEN QUERY BRW_PLAN FOR EACH faktstarttemp WHERE 
            faktstarttemp.FAKTNR = infakplannr AND faktstarttemp.START = ""
            NO-LOCK.            
         END.   
      END.
      IF rundavar = TRUE THEN DO:
         ASSIGN
         gfaktemp.TOTALT = runda(gfaktemp.TOTALT).
      END.
      GET FIRST BRW_PLAN NO-LOCK.   
      ENABLE BRW_PLAN WITH FRAME {&FRAME-NAME}.   
      RUN open_UI.         
      RAD_VISA = 4.         
      RUN visa_UI (INPUT TRUE).
   END.    
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
   {muswait.i}
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
   IF medvar = FALSE THEN RETURN.
   IF vartyp = 3 OR vartyp = 1 OR vartyp = 2  OR vartyp = 52 THEN RETURN.

   FIND FIRST gfaktemp WHERE gfaktemp.ORDNING = 3 NO-ERROR.
   ASSIGN   
   gfaktemp.ARBKOST = (gfaktemp.ARBKOST - gbelopp) +  nybelopp  
   gfaktemp.OBELOPP = (gfaktemp.OBELOPP - gobelopp) + nyobelopp    
   gfaktemp.RES = (gfaktemp.RES - gres) + nyres .
   /*HÄRKOST
   gfaktemp.KBELOPP = (gfaktemp.KBELOPP - gkbelopp) + nykbelopp.*/
   IF typvar = 1 THEN gfaktemp.MTRL = (gfaktemp.MTRL - gkbelopp) + nykbelopp.
   IF typvar = 2 THEN gfaktemp.OVRIG = (gfaktemp.OVRIG - gkbelopp) + nykbelopp.
   IF typvar = 3 THEN gfaktemp.ARBKOST = (gfaktemp.ARBKOST - gkbelopp) + nykbelopp.
   IF typvar = 4 THEN gfaktemp.TRAKT = (gfaktemp.TRAKT - gkbelopp) + nykbelopp.
   IF typvar = 5 THEN gfaktemp.KBELOPP = (gfaktemp.KBELOPP - gkbelopp) + nykbelopp.
   /*ANNARS ÄR TYPVAR = 0*/
   IF rundavar = TRUE THEN DO:
      ASSIGN
      gfaktemp.TOTALT = runda(gfaktemp.TOTALT)
      gfaktemp.RES = runda(gfaktemp.RES) 
      gfaktemp.ARBKOST = runda(gfaktemp.ARBKOST) 
      gfaktemp.LONTILL = runda(gfaktemp.LONTILL) 
      gfaktemp.MTRL = runda(gfaktemp.MTRL) 
      gfaktemp.OBELOPP = runda(gfaktemp.OBELOPP) 
      gfaktemp.OVRIG = runda(gfaktemp.OVRIG) 
      gfaktemp.TRAKT = runda(gfaktemp.TRAKT)       
      gfaktemp.KBELOPP = runda(gfaktemp.KBELOPP).
   END.    
   gfaktemp.TOTALT = gfaktemp.MTRL + gfaktemp.OVRIG + gfaktemp.ARBKOST + 
                        gfaktemp.OBELOPP + gfaktemp.TRAKT + gfaktemp.RES + 
                        gfaktemp.LONTILL + gfaktemp.KBELOPP.
   IF rundavar = TRUE THEN DO:
      ASSIGN
      gfaktemp.TOTALT = runda(gfaktemp.TOTALT).
   END.
   RUN refreshbrw_UI IN brwproc[6].
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fribort_UI WINDOW-3 
PROCEDURE fribort_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF TOG_MOMS = TRUE THEN DO:
      RUN kontokoll_UI.
      RETURN.
   END.
   status-ok = BRW_PLAN:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF NOT AVAILABLE faktfriatemp THEN RETURN.
   IF faktfriatemp.FAKTTEXT = "Dubbel-klicka på denna rad för nyupplägg" THEN DO:
      MESSAGE "Du kan inte ta bort denna post!" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   MESSAGE "Vill du ta bort denna post ?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val1 AS LOGICAL.
   CASE val1:
      WHEN TRUE THEN DO:
         
      END.
      WHEN FALSE THEN DO:
         RETURN.                   
      END.
   END CASE.
   /*BORTAG JA*/
   IF faktfriatemp.FAKTURERAD = TRUE THEN DO:
      FIND FIRST gfaktemp WHERE gfaktemp.ORDNING = 3 NO-ERROR.
      ASSIGN         
      gfaktemp.TOTALT = gfaktemp.TOTALT - faktfriatemp.TOTALT.
      RUN gladda_UI (INPUT 2).
   END.
   RUN fribortk_UI IN faktupphmth (INPUT faktfriatemp.ROWFRI).
   DELETE faktfriatemp.
   RUN selnextprevrow_UI IN brwproc[1].
   RUN refreshbrw_UI IN brwproc[1].
   RUN lastselectdyn_UI IN brwproc[1].                
   RUN refreshbrw_UI IN brwproc[6].
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
   IF val = 1 THEN DO:
      IF faktfriatemp.TYP BEGINS "PERS" THEN DO:
         gfaktemp.ARBKOST = gfaktemp.ARBKOST + faktfriatemp.TOTALT.
      END.
      IF faktfriatemp.TYP = "MASK" THEN DO:
         gfaktemp.KBELOPP = gfaktemp.KBELOPP + faktfriatemp.TOTALT.
      END.
      IF faktfriatemp.TYP = "MIL" THEN DO:
         gfaktemp.RES = gfaktemp.RES + faktfriatemp.TOTALT.
      END.
      IF faktfriatemp.TYP = "TRAKT" THEN DO:
         gfaktemp.TRAKT = gfaktemp.TRAKT + faktfriatemp.TOTALT.
      END.
      IF faktfriatemp.TYP = "OVER" OR faktfriatemp.TYP BEGINS "FAKT" THEN DO:
         gfaktemp.OVRIG = gfaktemp.OVRIG + faktfriatemp.TOTALT.
      END.
      IF faktfriatemp.TYP = "MATRL" THEN DO:
         gfaktemp.MTRL = gfaktemp.MTRL + faktfriatemp.TOTALT.
      END.
   END.
   IF val = 2 THEN DO:
      IF faktfriatemp.TYP BEGINS "PERS" THEN DO:
         gfaktemp.ARBKOST = gfaktemp.ARBKOST - faktfriatemp.TOTALT.
      END.
      IF faktfriatemp.TYP = "MASK" THEN DO:
         gfaktemp.KBELOPP = gfaktemp.KBELOPP - faktfriatemp.TOTALT.
      END.
      IF faktfriatemp.TYP = "MIL" THEN DO:
         gfaktemp.RES = gfaktemp.RES - faktfriatemp.TOTALT.
      END.
      IF faktfriatemp.TYP = "TRAKT" THEN DO:
         gfaktemp.TRAKT = gfaktemp.TRAKT - faktfriatemp.TOTALT.
      END.
      IF faktfriatemp.TYP = "OVER" OR faktfriatemp.TYP BEGINS "FAKT" THEN DO:
         gfaktemp.OVRIG = gfaktemp.OVRIG - faktfriatemp.TOTALT.
      END.
      IF faktfriatemp.TYP = "MATRL" THEN DO:
         gfaktemp.MTRL = gfaktemp.MTRL - faktfriatemp.TOTALT.
      END. 
   END.  
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
   ASSIGN
   BRW_FRI:HIDDEN IN FRAME {&FRAME-NAME} = gomavar   
   BTN_FRINY:HIDDEN IN FRAME {&FRAME-NAME} = gomavar   
   BTN_FRIAND:HIDDEN IN FRAME {&FRAME-NAME} = gomavar   
   BTN_FRIBORT:HIDDEN IN FRAME {&FRAME-NAME} = gomavar.
   IF TOG_MOMS = TRUE THEN DO:
      ASSIGN
      BTN_FRINY:HIDDEN IN FRAME {&FRAME-NAME} = TRUE   
      BTN_FRIAND:HIDDEN IN FRAME {&FRAME-NAME} = TRUE   
      BTN_FRIBORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
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
   IF musz = TRUE THEN DO:
      RETURN.
   END.
   WINDOW-3:TITLE = "Kreditfakturering".
   gfaktemp.KBELOPP:LABEL IN BROWSE BRW_GKOST = "Konterat!belopp".
   faktskap = FALSE.
   FIND FIRST vfaktplantemp WHERE vfaktplantemp.FAKTNR = infakplannr NO-LOCK NO-ERROR.     
   FIND FIRST faktyptemp WHERE faktyptemp.FAKTTYP = vfaktplantemp.FAKTTYP NO-ERROR.
   vartyp = faktyptemp.TYP.      
   IF efaktkredtemp.VFAKTNR = 0 THEN vartyp = 6.
   /*ccccHÄR */
   RUN hamtfaktk_UI IN faktupphmth 
   (INPUT infakplannr,INPUT FILL-IN_FDELNR,INPUT vartyp,
   OUTPUT TABLE faktaonrtemp,OUTPUT TABLE faktfriatemp,
   OUTPUT TABLE kundregeltemp,OUTPUT TABLE faktintakkontotemp,OUTPUT TABLE kundbeftemp,
   OUTPUT TABLE faktupparbtemp,OUTPUT TABLE faktstarttemp,OUTPUT TABLE kundovertemp).
   FIND FIRST faktureringstyptemp WHERE faktureringstyptemp.FAKTTYPID = efaktkredtemp.FAKTTYPID NO-LOCK NO-ERROR.
   IF AVAILABLE faktureringstyptemp THEN DO:
      IF faktureringstyptemp.TIDIGAREACONTO = TRUE AND faktureringstyptemp.SLUT THEN vartyp = 4.
      ELSE IF faktureringstyptemp.TIDIGAREACONTO = TRUE AND 
      faktureringstyptemp.SLUT = FALSE THEN vartyp = 3.
   END.
   IF vartyp = 5 THEN DO:      
      IF vfaktplantemp.FAKTTYPUNDER = 2 OR vfaktplantemp.FAKTTYPUNDER = 4 THEN vartyp = 52.   
   END.
   status-ok = RAD_VISA:ADD-LAST("Summerad faktura",5) IN FRAME {&FRAME-NAME}.             
   IF efaktkredtemp.VFAKTNR NE 0 THEN DO:      
      IF vartyp = 1 OR vartyp = 2 OR vartyp = 3 THEN DO:      
         status-ok = RAD_VISA:ADD-LAST("Faktureringstidpunkter",4) IN FRAME {&FRAME-NAME}.      
      END.
      ELSE IF vartyp = 52 THEN DO:      
         status-ok = RAD_VISA:ADD-LAST("Upparbetadekostnader",6).
      END.
      ELSE IF vartyp = 6 OR vartyp = 7 THEN DO:
         
      END.
      ELSE DO:
        /* status-ok = RAD_VISA:ADD-LAST("Tidskrivning",1).
         status-ok = RAD_VISA:DELETE("Fri komplettering").
         status-ok = RAD_VISA:ADD-LAST("Kostnadsreg.",2).
         status-ok = RAD_VISA:ADD-LAST("Fri komplettering",3).             
         status-ok = RAD_VISA:ADD-LAST("Summerad tidskrivning",5).      
         */
         
      END. 
      IF vartyp = 4 OR vartyp = 5 OR vartyp = 8 THEN DO:               
         RUN lop_UI.        
      END.
      ELSE IF vartyp = 6 OR vartyp = 7 THEN DO:
      END.
      ELSE DO:                 
         RUN fastpris_UI.            
      END.
      
   END.
   RUN huvud_UI. 
   {musarrow.i}
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
   FIND FIRST gfaktemp WHERE gfaktemp.ORDNING = 3 NO-ERROR.
   IF NOT AVAILABLE gfaktemp THEN CREATE gfaktemp.
   ASSIGN
   gfaktemp.TYPTEXT = "Denna faktura" 
   gfaktemp.ORDNING = 3.        
   /*
   OPEN QUERY BRW_FRI FOR EACH faktfriatemp WHERE 
   faktfriatemp.FAKTNR = infakplannr AND
   faktfriatemp.FDELNR = FILL-IN_FDELNR NO-LOCK BY faktfriatemp.TYP.                           
   */
   RUN openbdynspec_UI IN brwproc[1].
   GET FIRST BRW_FRI NO-LOCK.   
   DO WHILE AVAILABLE(faktfriatemp):                              
      IF faktfriatemp.FAKTURERA = TRUE THEN DO:
         /*HÄRFRI*/
         /* gfaktemp.OVRIG = gfaktemp.OVRIG + FAKTFRIA.TOTALT.  */
         RUN gladda_UI (INPUT 1).                                   
      END.
      GET NEXT BRW_FRI NO-LOCK.   
   END.
   FIND FIRST faktfriatemp WHERE faktfriatemp.FAKTTEXT = "Dubbel-klicka på denna rad för nyupplägg" NO-LOCK NO-ERROR.
   IF AVAILABLE faktfriatemp THEN DELETE faktfriatemp.
   /*
   IF NOT AVAILABLE faktfriatemp THEN DO: 
      CREATE faktfriatemp.   
      ASSIGN                                    
      faktfriatemp.FAKTNR = infakplannr 
      faktfriatemp.FDELNR = FILL-IN_FDELNR
      faktfriatemp.FAKTURERAD = FALSE
      faktfriatemp.FAKTTEXT = "Dubbel-klicka på denna rad för nyupplägg".                               
   END.
   */
   /*
   OPEN QUERY BRW_FRI FOR EACH faktfriatemp WHERE 
   faktfriatemp.FAKTNR = infakplannr AND
   faktfriatemp.FDELNR = FILL-IN_FDELNR NO-LOCK BY faktfriatemp.TYP.
   */
   RUN openbdynspec_UI IN brwproc[1].
   GET FIRST BRW_FRI NO-LOCK.
   IF vartyp = 1 OR vartyp = 2 OR vartyp = 52 THEN DO:      
      gfaktemp.TOTALT = gfaktemp.TOTALT + 
                        gfaktemp.MTRL + gfaktemp.OVRIG + gfaktemp.ARBKOST + 
                        gfaktemp.OBELOPP + gfaktemp.TRAKT + gfaktemp.RES + 
                        gfaktemp.LONTILL + gfaktemp.KBELOPP.
      IF rundavar = TRUE THEN DO:
         ASSIGN
         gfaktemp.TOTALT = runda(gfaktemp.TOTALT).
      END.                           
   END.
   ELSE IF vartyp = 3 THEN DO:      
      gfaktemp.TOTALT = gfaktemp.TOTALT + gfaktemp.OVRIG.
      IF rundavar = TRUE THEN DO:
         ASSIGN
         gfaktemp.TOTALT = runda(gfaktemp.TOTALT).
      END.                           
   END.
   ELSE DO:      
      gfaktemp.TOTALT = gfaktemp.MTRL + gfaktemp.OVRIG + gfaktemp.ARBKOST + 
                        gfaktemp.OBELOPP + gfaktemp.TRAKT + gfaktemp.RES + 
                        gfaktemp.LONTILL + gfaktemp.KBELOPP.
      IF rundavar = TRUE THEN DO:
         ASSIGN
         gfaktemp.TOTALT = runda(gfaktemp.TOTALT).
      END.                  
   END.
   
   ENABLE BRW_FRI WITH FRAME {&FRAME-NAME}.   
   ASSIGN
   BRW_PLAN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   BRW_KOST:HIDDEN IN FRAME {&FRAME-NAME} = TRUE  
   BRW_TID:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BRW_ALLT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.  
   RUN gomafri_UI (INPUT TRUE).
   IF RAD_VISA = 0 THEN RAD_VISA = 5.
   IF RAD_VISA = 1 THEN DO:
      DISPLAY RAD_VISA WITH FRAME {&FRAME-NAME}.
      ENABLE RAD_VISA WITH FRAME {&FRAME-NAME}. 
      ASSIGN
      BRW_TID:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.      
   END.    
   ELSE IF RAD_VISA = 2 THEN DO:                   
      DISPLAY RAD_VISA WITH FRAME {&FRAME-NAME}.
      ENABLE RAD_VISA WITH FRAME {&FRAME-NAME}. 
      ASSIGN
      BRW_KOST:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.      
   END.
   ELSE IF RAD_VISA = 3 THEN DO:                  
      DISPLAY RAD_VISA WITH FRAME {&FRAME-NAME}.
      ENABLE RAD_VISA WITH FRAME {&FRAME-NAME}. 
      RUN gomafri_UI (INPUT FALSE).      
   END.   
   ELSE IF RAD_VISA = 4 THEN DO:                  
      DISPLAY RAD_VISA WITH FRAME {&FRAME-NAME}.
      ENABLE RAD_VISA WITH FRAME {&FRAME-NAME}. 
      ASSIGN
      BRW_PLAN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.                            
   END. 
   ELSE IF RAD_VISA = 5 THEN DO:                  
         DISPLAY RAD_VISA WITH FRAME {&FRAME-NAME}.
      ENABLE RAD_VISA WITH FRAME {&FRAME-NAME}. 
     /* ASSIGN
      BRW_PLAN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE. 
      */                           
   END.
   ELSE IF RAD_VISA = 6 THEN DO:                  
      DISPLAY RAD_VISA WITH FRAME {&FRAME-NAME}.
      ENABLE RAD_VISA WITH FRAME {&FRAME-NAME}. 
      ASSIGN
      BRW_UPP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.                            
   END. 
   RUN enable_UI.      
   FIND FIRST faktintakkontotemp WHERE 
   faktintakkontotemp.FAKTNR = infakplannr AND  
   faktintakkontotemp.FDELNR = FILL-IN_FDELNR 
   NO-LOCK NO-ERROR.
   
   IF AVAILABLE faktintakkontotemp THEN DO:
      TOG_MOMS = TRUE.
      ASSIGN
      FBTN_MOMS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
      FBTN_MOMSBORT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      TOG_MOMS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      /*
      DISPLAY TOG_MOMS WITH FRAME {&FRAME-NAME}.
      ENABLE TOG_MOMS WITH FRAME {&FRAME-NAME}.
      */
   END.
   ELSE DO:
      ASSIGN
      FBTN_MOMS:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      FBTN_MOMSBORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      TOG_MOMS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   RUN gomafri_UI (INPUT TRUE).
   IF TOG_MOMS = FALSE THEN DO:
      IF (vartyp = 4 OR vartyp = 5) THEN DO:
         valaconttak = TRUE.                 
      END.
      IF vartyp = 8 THEN DO: 
      END.      
   END.    
   IF vartyp = 3 OR vartyp = 4 OR vartyp = 5 OR vartyp = 8 THEN DO:
      RAD_VISA = 5. 
      DISPLAY RAD_VISA WITH FRAME {&FRAME-NAME}.
      APPLY "VALUE-CHANGED" TO RAD_VISA.
   END.
   IF vartyp = 6 OR vartyp = 7 THEN DO:
      status-ok = RAD_VISA:DELETE("Tidskrivning").
      status-ok = RAD_VISA:DELETE("Kostnadsreg.").            
   END.
   IF vartyp = 1 OR vartyp = 2 OR vartyp = 3  OR vartyp = 52 OR vartyp = 6 OR vartyp = 7 THEN TOG_DIFF:HIDDEN = TRUE.
   ELSE IF RAD_VISA NE 2 THEN TOG_DIFF:HIDDEN = TRUE.
   /*faktfor*/
   IF skarpfaktok = FALSE THEN DO:
      FBTN_OK:LABEL = "Godkänn faktura" .
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
         FBTN_OK:LABEL = "Klar till beställare" .
      END.  
   END.
   
   
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
   /*FAKTKOLL OCH SLUT FAKT*/ 
   DEFINE VARIABLE utfil AS CHARACTER NO-UNDO.
   DEFINE VARIABLE svareko AS LOGICAL NO-UNDO.
   DEFINE VARIABLE answeradm  AS LOGICAL NO-UNDO.
   DEFINE VARIABLE answerprel AS LOGICAL NO-UNDO.
   DEFINE VARIABLE answerFB AS LOGICAL NO-UNDO.
   DEFINE VARIABLE answerskriv AS LOGICAL NO-UNDO.
   DEFINE VARIABLE varfakturd AS DATE NO-UNDO.
   DEFINE VARIABLE varforfalld AS DATE NO-UNDO.
   DEFINE VARIABLE varbokdatum AS DATE NO-UNDO.
   
   IF TOG_MOMS = FALSE THEN DO:
      MESSAGE "Du har inte gjort kontoberäkningar." 
      VIEW-AS ALERT-BOX TITLE "Fakturera".
      RETURN.
   END.
   FIND FIRST kundregeltemp WHERE kundregeltemp.FAKTNR = vfaktplantemp.FAKTNR 
   NO-LOCK NO-ERROR.
   ASSIGN
   varbokdatum = DATE(MONTH(TODAY),01,YEAR(TODAY)) - 1.      
   varforfalld = TODAY + kundregeltemp.FDAGAR.
   varfakturd = TODAY.
   
   RUN forfaldatum_UI IN faktupphmth (INPUT-OUTPUT varforfalld).
   EMPTY TEMP-TABLE faktkredtemp NO-ERROR. 
   RUN hamtfaktku_UI IN faktupphmth 
   (INPUT infakplannr,INPUT FILL-IN_FDELNR,OUTPUT TABLE efaktkredtemp).
   FIND FIRST efaktkredtemp NO-LOCK NO-ERROR.
   CREATE faktkredtemp.
   BUFFER-COPY efaktkredtemp TO faktkredtemp.
   RUN BILAGDATUMA.W (INPUT infakplannr,INPUT FILL-IN_FDELNR,INPUT gamanv,
                      INPUT FALSE,INPUT-OUTPUT varfakturd,INPUT-OUTPUT varforfalld,INPUT-OUTPUT varbokdatum,
                      OUTPUT answeradm,OUTPUT answerprel,OUTPUT answerskriv,OUTPUT answerFB,
                      INPUT-OUTPUT TABLE faktureradtemp,INPUT-OUTPUT TABLE faktkredtemp).
   FIND FIRST faktkredtemp WHERE faktkredtemp.FAKTNR = infakplannr AND
   faktkredtemp.FDELNR = FILL-IN_FDELNR NO-LOCK NO-ERROR.
   BUFFER-COPY faktkredtemp TO efaktkredtemp.
   EMPTY TEMP-TABLE faktkredtemp NO-ERROR. 
   IF musz = TRUE THEN DO:
      musz = FALSE.
      RETURN.
   END.
   IF skarpfaktok = FALSE AND Guru.Konstanter:faktsekvar[5] = FALSE THEN DO:   
      ASSIGN 
      efaktkredtemp.PRELGOD = TRUE
      efaktkredtemp.DATUM = TODAY  
      efaktkredtemp.BOKDATUM = varbokdatum.      
      RUN sparfaktkkred_UI  IN faktupphmth (INPUT TABLE efaktkredtemp).
   END.
   ELSE DO:     
      RUN skarpnr_UI IN faktupphmth (INPUT infakplannr,OUTPUT skarpvar,OUTPUT feltextvar).
      IF feltextvar NE "" THEN DO:
         MESSAGE feltextvar VIEW-AS ALERT-BOX.
         RETURN.
      END.
      FIND FIRST gfaktempbuff WHERE gfaktempbuff.ORDNING = 3 NO-LOCK NO-ERROR.
      IF vartyp = 3 OR vartyp = 1 THEN DO:
         GET FIRST BRW_PLAN. 
         DO WHILE AVAILABLE(faktstarttemp):
                             
            IF faktstarttemp.VKREDIT = 0 AND faktstarttemp.FAKTURERAD = TRUE THEN DO:
               faktstarttemp.VKREDIT = skarpvar.
            END.
            GET NEXT BRW_PLAN.            
         END.
      END.
      ELSE IF vartyp = 2 THEN DO: 
         GET FIRST BRW_PLAN. 
         DO WHILE AVAILABLE(faktstarttemp):
            IF faktstarttemp.VKREDIT = 0 AND faktstarttemp.FAKTURERAD = TRUE THEN DO:
               faktstarttemp.VKREDIT = skarpvar.
            END.
            GET NEXT BRW_PLAN.         
         END.     
      END.
      ELSE IF vartyp = 52 THEN DO: 
         GET FIRST BRW_UPP. 
         DO WHILE AVAILABLE(faktupparbtemp):
            IF faktupparbtemp.VKREDIT = 0 AND faktupparbtemp.FAKTURERAD = TRUE THEN DO:
               faktupparbtemp.VKREDIT = skarpvar.
            END.
            GET NEXT BRW_UPP.         
         END.              
      END.
      ELSE DO:
         
         
      END.                      
      {muswait.i}          
      faktskap = TRUE.                
      status-mus2 = SESSION:SET-WAIT-STATE("GENERAL").     
      ASSIGN 
      efaktkredtemp.PRELGOD = FALSE
      efaktkredtemp.DATUM = TODAY  
      efaktkredtemp.BOKDATUM = varbokdatum.      
      
      FOR EACH faktfriatemp:                                               
         IF faktfriatemp.FAKTTEXT = "Dubbel-klicka på denna rad för nyupplägg" THEN musz = musz.
         ELSE DO:
            ASSIGN
            faktfriatemp.FAKTNR = vfaktplantemp.FAKTNR
            faktfriatemp.FDELNR = FILL-IN_FDELNR
            faktfriatemp.VKREDIT = skarpvar.   
         END.         
      END.   
      RUN skarpsattk_UI IN faktupphmth (INPUT infakplannr,INPUT FILL-IN_FDELNR,INPUT skarpvar,
      INPUT varbokdatum,INPUT varfakturd,INPUT varforfalld).             
      RUN sparfaktkkred_UI  IN faktupphmth (INPUT TABLE efaktkredtemp).
      RUN spara_UI (INPUT skarpvar).      
   END.
   IF skarpfaktok = TRUE OR Guru.Konstanter:faktsekvar[5] = TRUE THEN DO:        
      svareko = TRUE.
      IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
         FIND FIRST kundregeltemp WHERE kundregeltemp.FAKTNR = vfaktplantemp.FAKTNR 
         NO-LOCK NO-ERROR.
         IF kundregeltemp.KUNDID = 1 OR kundregeltemp.KUNDID = 2 THEN DO:
            MESSAGE "Vill du läsa över dessa poster till ekonomisystemet?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE svareko.
            CASE svareko:
               WHEN TRUE THEN DO:
                  svareko = TRUE.          
               END.
               WHEN FALSE THEN DO:
                  svareko = FALSE.          
               END.
            END CASE.  
         END.     
      END.
      IF svareko THEN DO:
         IF Guru.Konstanter:appcon THEN DO:
            RUN FAKKOAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
            (INPUT TRUE, INPUT vfaktplantemp.FAKTNR, INPUT FILL-IN_FDELNR, INPUT skarpvar,OUTPUT TABLE felmeddtemp).
         END.
         ELSE DO:
            RUN FAKKOAPP.P  
            (INPUT TRUE, INPUT vfaktplantemp.FAKTNR, INPUT FILL-IN_FDELNR, INPUT skarpvar,OUTPUT TABLE felmeddtemp).
         END.
         FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
         IF AVAILABLE felmeddtemp THEN DO:
            utfil =  SESSION:TEMP-DIR + "faktfel.txt".
            {SESSIONTEMPDIR.I}
            IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN utfil = webclienttempdir + "faktfel.txt".
            OUTPUT TO VALUE(utfil).
            PUT UNFORMATTED "KONTAKTA ELPOOL 090-184540 DU FICK NÅGRA FEL VID ÖVERLÄSNINGEN TILL EKONOMISYSTEMET. INGET ÄR ÖVERLÄST TILL EKONOMI" SKIP.
            FOR EACH felmeddtemp:
               PUT UNFORMATTED felmeddtemp.FELMEDD SKIP.
            END.
            OUTPUT CLOSE.
            RUN OPENDOC.P (utfil,"","",FALSE).            
         END.
      END.
   END.
   RUN medskickk_UI IN faktupphmth (INPUT infakplannr,INPUT FILL-IN_FDELNR,INPUT skarpvar,
   INPUT answeradm,INPUT answerprel,INPUT answerFB,INPUT gamanv,INPUT Guru.Konstanter:globanv,INPUT FALSE).     
   RUN gomafri_UI (INPUT TRUE).
   APPLY "VALUE-CHANGED" TO RAD_VISA IN FRAME {&FRAME-NAME}.
   ASSIGN
   FBTN_MOMSBORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FBTN_MOMS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FBTN_OK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   TOG_MOMS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   {musarrow.i}         
   MESSAGE "Fakturan klar!" VIEW-AS ALERT-BOX.
   IF answerskriv = TRUE THEN DO:        
      APPLY "CHOOSE" TO FBTN_VISFAK.
   END.      
             
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
   gfaktemp.TYPTEXT = "Denna faktura" 
   gfaktemp.ORDNING = 3.
   IF Guru.Konstanter:appcon THEN DO:
      RUN FAKOSKAPPA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT infakplannr,INPUT kreditnrvar,OUTPUT TABLE kosttemp).
   END.
   ELSE DO:
      RUN FAKOSKAPPA.P 
      (INPUT infakplannr,INPUT kreditnrvar,OUTPUT TABLE kosttemp).
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
   {muswait.i}
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
   IF TOG_MOMS = TRUE THEN DO:
      RUN kontokoll_UI.
      DISPLAY kosttemp.MED WITH BROWSE BRW_KOST.
   END.
   ELSE DO:   
      IF vartyp = 1 OR vartyp = 2 OR vartyp = 52 THEN musz = musz.
      ELSE IF vartyp = 3 THEN musz = musz.
      ELSE DO:      
         brwrow = ROWID(kosttemp).          
         FIND FIRST gfaktemp WHERE gfaktemp.ORDNING = 3 NO-ERROR.
         IF medraknare = 1 THEN DO:
            status-ok = BRW_KOST:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
            IF NOT AVAILABLE kosttemp THEN RETURN.
            IF kosttemp.MED = TRUE THEN DO:                    
               ASSIGN
               gfaktemp.MTRL = gfaktemp.MTRL - (kosttemp.MTRLPAKR + kosttemp.MTRL)
               gfaktemp.OVRIG = gfaktemp.OVRIG - kosttemp.OVRKR
               gfaktemp.ARBKOST = gfaktemp.ARBKOST - kosttemp.PERSKOST 
               gfaktemp.TRAKT = gfaktemp.TRAKT - kosttemp.TRAKTKOST  
               gfaktemp.KBELOPP = gfaktemp.KBELOPP - (kosttemp.FRTJPAKR + kosttemp.MASKKOST).                                         
               kosttemp.MED = FALSE.  
            END.
            ELSE IF kosttemp.MED = FALSE THEN kosttemp.MED = ?.
            ELSE IF kosttemp.MED = ? THEN DO:
               ASSIGN
               gfaktemp.MTRL = gfaktemp.MTRL + (kosttemp.MTRLPAKR + kosttemp.MTRL)
               gfaktemp.OVRIG = gfaktemp.OVRIG + kosttemp.OVRKR
               gfaktemp.ARBKOST = gfaktemp.ARBKOST + kosttemp.PERSKOST 
               gfaktemp.TRAKT = gfaktemp.TRAKT + kosttemp.TRAKTKOST  
               gfaktemp.KBELOPP = gfaktemp.KBELOPP + (kosttemp.FRTJPAKR + kosttemp.MASKKOST).
               kosttemp.MED = TRUE.
            END.
         END.
         IF medraknare = 2 THEN DO:
            status-ok = BRW_KOST:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
            IF NOT AVAILABLE kosttemp THEN RETURN.
            IF kosttemp.MED = TRUE THEN DO:                    
               ASSIGN
               gfaktemp.MTRL = gfaktemp.MTRL - (kosttemp.MTRLPAKR + kosttemp.MTRL)
               gfaktemp.OVRIG = gfaktemp.OVRIG - kosttemp.OVRKR
               gfaktemp.ARBKOST = gfaktemp.ARBKOST - kosttemp.PERSKOST 
               gfaktemp.TRAKT = gfaktemp.TRAKT - kosttemp.TRAKTKOST  
               gfaktemp.KBELOPP = gfaktemp.KBELOPP - (kosttemp.FRTJPAKR + kosttemp.MASKKOST).                                                        
            END.
            ELSE DO:               
               IF INPUT BROWSE BRW_KOST kosttemp.MED = TRUE THEN DO:           
                  ASSIGN
                  gfaktemp.MTRL = gfaktemp.MTRL + (kosttemp.MTRLPAKR + kosttemp.MTRL)
                  gfaktemp.OVRIG = gfaktemp.OVRIG + kosttemp.OVRKR
                  gfaktemp.ARBKOST = gfaktemp.ARBKOST + kosttemp.PERSKOST 
                  gfaktemp.TRAKT = gfaktemp.TRAKT + kosttemp.TRAKTKOST  
                  gfaktemp.KBELOPP = gfaktemp.KBELOPP + (kosttemp.FRTJPAKR + kosttemp.MASKKOST).
               END.
            END.
            kosttemp.MED = INPUT BROWSE BRW_KOST kosttemp.MED.
         END.
         IF medraknare = 3 THEN DO:
            IF kosttemp.MED = TRUE THEN DO:                    
               ASSIGN
               gfaktemp.MTRL = gfaktemp.MTRL + (kosttemp.MTRLPAKR + kosttemp.MTRL)
               gfaktemp.OVRIG = gfaktemp.OVRIG + kosttemp.OVRKR
               gfaktemp.ARBKOST = gfaktemp.ARBKOST + kosttemp.PERSKOST 
               gfaktemp.TRAKT = gfaktemp.TRAKT + kosttemp.TRAKTKOST  
               gfaktemp.KBELOPP = gfaktemp.KBELOPP + (kosttemp.FRTJPAKR + kosttemp.MASKKOST).                                                        
            END.
            ELSE IF kosttemp.MED = FALSE THEN DO:
               ASSIGN
               gfaktemp.MTRL = gfaktemp.MTRL - (kosttemp.MTRLPAKR + kosttemp.MTRL)
               gfaktemp.OVRIG = gfaktemp.OVRIG - kosttemp.OVRKR
               gfaktemp.ARBKOST = gfaktemp.ARBKOST - kosttemp.PERSKOST 
               gfaktemp.TRAKT = gfaktemp.TRAKT - kosttemp.TRAKTKOST  
               gfaktemp.KBELOPP = gfaktemp.KBELOPP - (kosttemp.FRTJPAKR + kosttemp.MASKKOST).               
            END.
            RUN berraknatid_UI.
            RUN refreshbrw_UI IN brwproc[6].
            RETURN.
         END.
         IF medraknare = 4 THEN DO:
            IF kosttemp.MED = TRUE THEN DO:                    
               ASSIGN
               gfaktemp.MTRL = gfaktemp.MTRL - (kosttemp.MTRLPAKR + kosttemp.MTRL)
               gfaktemp.OVRIG = gfaktemp.OVRIG - kosttemp.OVRKR
               gfaktemp.ARBKOST = gfaktemp.ARBKOST - kosttemp.PERSKOST 
               gfaktemp.TRAKT = gfaktemp.TRAKT - kosttemp.TRAKTKOST  
               gfaktemp.KBELOPP = gfaktemp.KBELOPP - (kosttemp.FRTJPAKR + kosttemp.MASKKOST).                                                        
            END.
            ELSE IF kosttemp.MED = FALSE OR kosttemp.MED = ? THEN DO:
               ASSIGN
               gfaktemp.MTRL = gfaktemp.MTRL + (kosttemp.MTRLPAKR + kosttemp.MTRL)
               gfaktemp.OVRIG = gfaktemp.OVRIG + kosttemp.OVRKR
               gfaktemp.ARBKOST = gfaktemp.ARBKOST + kosttemp.PERSKOST 
               gfaktemp.TRAKT = gfaktemp.TRAKT + kosttemp.TRAKTKOST  
               gfaktemp.KBELOPP = gfaktemp.KBELOPP + (kosttemp.FRTJPAKR + kosttemp.MASKKOST).               
            END.
            RUN refreshbrw_UI IN brwproc[6].
            RETURN.
         END.
         RUN openkost_UI (INPUT 1).  
         RUN refreshbrw_UI IN brwproc[6].
         RUN setlastrowid_UI IN brwproc[8] (INPUT brwrow).                  
         RUN lastselectdyn_UI IN brwproc[8].               
      END.
   END.
   {musarrow.i}
   /*S
   DISPLAY kosttemp.MED WITH BROWSE BRW_KOST.
   */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE medplan_UI WINDOW-3 
PROCEDURE medplan_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER medraknare AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER varbrobwser AS CHARACTER NO-UNDO.
   DEFINE VARIABLE varslutbelopp AS DECIMAL NO-UNDO.
   IF varbrobwser = "PLAN" THEN DO:   
      IF TOG_MOMS = TRUE THEN DO:
         RUN kontokoll_UI.
         RUN refreshbrw_UI IN brwproc[2].
         RETURN.         
      END.                       
      status-ok = BRW_PLAN:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      IF NOT AVAILABLE faktstarttemp THEN RETURN.
      FIND FIRST gfaktemp WHERE gfaktemp.ORDNING = 3 NO-ERROR. 
      IF faktstarttemp.FAKTURERAD = TRUE THEN DO:
         IF faktstarttemp.VKREDIT = 0 THEN DO:
            MESSAGE "Vill du verkligen ta bort fakturadatumet för denna post ?"   
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bortag av fakturadatum"
            UPDATE answer AS LOGICAL.
            IF answer THEN DO: 
               ASSIGN 
               faktstarttemp.FAKTURERAD = FALSE 
               faktstarttemp.FAKTURADATUM = ?.      
               gfaktemp.TOTALT = gfaktemp.TOTALT - faktstarttemp.BELOPP. 
               IF rundavar = TRUE THEN DO:
                 ASSIGN
                  gfaktemp.TOTALT = runda(gfaktemp.TOTALT).      
               END.             
            END.
         END.
         ELSE DO:
            MESSAGE "Det går inte att ta bort denna post då den redan är fakturerad"   
            VIEW-AS ALERT-BOX.
         END.      
      END.                             
      ELSE DO:
         RUN fakstartfak_UI.                                       
      END.
      RUN refreshbrw_UI IN brwproc[2].
      RUN refreshbrw_UI IN brwproc[6].
      DISPLAY faktstarttemp.FAKTURERAD faktstarttemp.FAKTURADATUM WITH BROWSE BRW_PLAN.
   END.
   ELSE IF varbrobwser = "UPP" THEN DO:   
      IF TOG_MOMS = TRUE THEN DO:
         RUN kontokoll_UI.
         RUN refreshbrw_UI IN brwproc[3].
         RETURN.
      END.
      status-ok = BRW_UPP:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
      IF NOT AVAILABLE faktupparbtemp THEN RETURN.
      FIND FIRST gfaktemp WHERE gfaktemp.ORDNING = 3 NO-ERROR. 
      IF faktupparbtemp.FAKTURERAD = TRUE THEN DO:
         IF faktupparbtemp.FAKTNR = 0 THEN DO:
            MESSAGE "Vill du verkligen ta bort fakturadatumet för denna post ?"   
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Bortag av fakturadatum"
            UPDATE answerupparb AS LOGICAL.
            IF answerupparb THEN DO: 
               ASSIGN 
               faktupparbtemp.FAKTURERAD = FALSE 
               faktupparbtemp.FAKTURADATUM = ?.      
               gfaktemp.TOTALT = gfaktemp.TOTALT - faktupparbtemp.FAKTBELOPP. 
               IF rundavar = TRUE THEN DO:
                 ASSIGN
                  gfaktemp.TOTALT = runda(gfaktemp.TOTALT).      
               END.
               
            END.
         END.
         ELSE DO:
            MESSAGE "Det går inte att ta bort denna post då den redan är fakturerad"   
            VIEW-AS ALERT-BOX.
         END.      
      END.                             
      ELSE DO:
         ASSIGN
         faktupparbtemp.FDELNR = FILL-IN_FDELNR
         faktupparbtemp.FAKTURADATUM = TODAY
         faktupparbtemp.FAKTURERAD = TRUE. 
         gfaktemp.TOTALT = gfaktemp.TOTALT + faktupparbtemp.FAKTBELOPP.
         IF rundavar = TRUE THEN DO:
            ASSIGN
            gfaktemp.TOTALT = runda(gfaktemp.TOTALT).
         END.      
      END.     
      RUN refreshbrw_UI IN brwproc[3].
      RUN refreshbrw_UI IN brwproc[6].
      
      DISPLAY faktupparbtemp.FAKTURERAD faktupparbtemp.FAKTURADATUM WITH BROWSE BRW_UPP.
   END.
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE medtid_UI WINDOW-3 
PROCEDURE medtid_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER medraknare AS INTEGER NO-UNDO.
   IF TOG_MOMS = TRUE THEN DO:
      RUN kontokoll_UI.
      DISPLAY sumtidtemp.MED WITH BROWSE BRW_TID.
   END.
   ELSE DO:
      IF vartyp = 1 OR vartyp = 2 OR vartyp = 52 THEN musz = musz.
      ELSE IF vartyp = 3 THEN musz = musz.
      ELSE DO:         
         status-ok = BRW_TID:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
         IF NOT AVAILABLE sumtidtemp THEN RETURN.
         brwrow = ROWID(sumtidtemp).                   
         IF sumtidtemp.MED = TRUE THEN DO:
            RUN bertidkost_UI (INPUT FALSE).
            IF medraknare = 1 THEN DO:
               IF sumtidtemp.MED = TRUE THEN sumtidtemp.MED = FALSE.
               ELSE IF sumtidtemp.MED = FALSE THEN sumtidtemp.MED = ?.
               ELSE IF sumtidtemp.MED = ? THEN sumtidtemp.MED = TRUE.              
            END. 
            ELSE DO:
               sumtidtemp.MED = INPUT sumtidtemp.MED.
            END.  
         END.
         ELSE IF sumtidtemp.MED = FALSE OR sumtidtemp.MED = ? THEN DO:
            IF medraknare = 1 THEN DO:
               IF sumtidtemp.MED = TRUE THEN sumtidtemp.MED = FALSE.
               ELSE IF sumtidtemp.MED = FALSE THEN sumtidtemp.MED = ?.
               ELSE IF sumtidtemp.MED = ? THEN sumtidtemp.MED = TRUE.              
            END. 
            ELSE DO:
               sumtidtemp.MED = INPUT sumtidtemp.MED.
            END.
            IF sumtidtemp.MED = TRUE THEN DO:
               RUN bertidkost_UI (INPUT TRUE).
            END.
         END.                     
         RUN refreshbrw_UI IN brwproc[6].
         RUN setlastrowid_UI IN brwproc[9] (INPUT brwrow).                  
         RUN lastselectdyn_UI IN brwproc[9].               
         DISPLAY sumtidtemp.MED WITH BROWSE BRW_TID.
      END.
   END.  
   {musarrow.i}
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
   {muswait.i}
   DEFINE INPUT PARAMETER fribortvar AS LOGICAL NO-UNDO.
   RUN momsbortk_UI IN faktupphmth (INPUT infakplannr,INPUT FILL-IN_FDELNR).
   IF fribortvar = TRUE THEN DO:
      FIND FIRST gfaktemp WHERE gfaktemp.ORDNING = 3 NO-ERROR.
      FOR EACH faktfriatemp WHERE faktfriatemp.FAKTNR = vfaktplantemp.FAKTNR AND
      faktfriatemp.FDELNR = efaktkredtemp.FDELNR AND faktfriatemp.FAKTTEXT = "Korigering mot takpris":
         gfaktemp.TOTALT = gfaktemp.TOTALT - faktfriatemp.TOTALT.
         RUN gladda_UI (INPUT 2).
         IF rundavar = TRUE THEN DO:
            ASSIGN
            gfaktemp.TOTALT = runda(gfaktemp.TOTALT).               
         END.            
         RUN fribortk_UI IN faktupphmth (INPUT faktfriatemp.ROWFRI).
         DELETE faktfriatemp.                    
      END.
      RUN refreshbrw_UI IN brwproc[6].
      RUN hamtfrik_UI IN faktupphmth (INPUT infakplannr,INPUT FILL-IN_FDELNR,OUTPUT TABLE faktfriatemp).
      FIND FIRST faktfriatemp WHERE faktfriatemp.FAKTTEXT = "Dubbel-klicka på denna rad för nyupplägg" NO-LOCK NO-ERROR.
      IF AVAILABLE faktfriatemp THEN DELETE faktfriatemp.
      /*
      IF NOT AVAILABLE faktfriatemp THEN DO: 
         CREATE faktfriatemp.   
         ASSIGN                                    
         faktfriatemp.FAKTNR = infakplannr 
         faktfriatemp.FDELNR = FILL-IN_FDELNR
         faktfriatemp.FAKTURERAD = FALSE
         faktfriatemp.FAKTTEXT = "Dubbel-klicka på denna rad för nyupplägg".                               
      END.
      */
      RUN openbdynspec_UI IN brwproc[1].
      RUN openbdyn_UI IN brwproc[2] (INPUT "").
      IF RAD_VISA = 5 THEN DO:
         APPLY "VALUE-CHANGED" TO RAD_VISA IN FRAME {&FRAME-NAME}.
      END.
   END.   
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
   {muswait.i}
      
   ASSIGN      
   BTN_BACK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BTN_OVER:HIDDEN = TRUE. 
   
   IF TOG_MOMS = FALSE THEN DO:      
      RUN spara_UI (INPUT skarpvar).                  
   END.   
   IF Guru.Konstanter:appcon THEN DO:
      RUN MOMSBERKA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT infakplannr, INPUT FILL-IN_FDELNR, INPUT vartyp, INPUT TOG_MOMS, INPUT skarpvar,
       INPUT tidfaktvar, INPUT tidmomsvar, INPUT rundavar,
       INPUT TABLE kosttemp, INPUT TABLE sumtidtemp, INPUT TABLE gfaktemp,
       OUTPUT TABLE kolltabeltemp).              
   END.
   ELSE DO:
      RUN MOMSBERKA.P 
      (INPUT infakplannr, INPUT FILL-IN_FDELNR, INPUT vartyp, INPUT TOG_MOMS, INPUT skarpvar,
       INPUT tidfaktvar, INPUT tidmomsvar, INPUT rundavar,
       INPUT TABLE kosttemp, INPUT TABLE sumtidtemp, INPUT TABLE gfaktemp,
       OUTPUT TABLE kolltabeltemp). 
   END.     
   RUN KONTBERKA.W (INPUT infakplannr, INPUT FILL-IN_FDELNR,INPUT TRUE,
   INPUT TABLE efaktkredtemp,INPUT TABLE faktaonrtemp).
   
   ASSIGN    
   TOG_MOMS = TRUE.
   ASSIGN
   FBTN_MOMS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   FBTN_MOMSBORT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
   TOG_MOMS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   IF efaktkredtemp.VKREDIT = 0 THEN DO:
      /*
      DISPLAY TOG_MOMS WITH FRAME {&FRAME-NAME}.
      ENABLE TOG_MOMS WITH FRAME {&FRAME-NAME}. 
      */
      IF Guru.Konstanter:faktsekvar[5] = TRUE THEN DO:
         ASSIGN
         FBTN_MOMSBORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         FBTN_MOMS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         TOG_MOMS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      END.
   END.
   
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
   FIND FIRST gfaktemp WHERE gfaktemp.ORDNING = 3 NO-ERROR.
   IF NOT AVAILABLE  gfaktemp THEN CREATE gfaktemp.
   ASSIGN
   gfaktemp.TYPTEXT = "Denna faktura" 
   gfaktemp.ORDNING = 3.      
   RUN stid_UI.
   RUN befat_UI.      
   gfaktemp.RES = 0.
   OPEN QUERY BRW_TID FOR EACH sumtidtemp NO-LOCK.
   ENABLE BRW_TID WITH FRAME {&FRAME-NAME}.   
   GET FIRST BRW_TID NO-LOCK.
   DO WHILE AVAILABLE(sumtidtemp):    
      RAD_VISA = 1.
      IF sumtidtemp.MED = TRUE THEN DO:                                               
         gfaktemp.ARBKOST = gfaktemp.ARBKOST + sumtidtemp.BELOPP.                             
         gfaktemp.OBELOPP = gfaktemp.OBELOPP + sumtidtemp.OBELOPP.                               
         gfaktemp.TRAKT = gfaktemp.TRAKT + sumtidtemp.TBELOPP.                            
         gfaktemp.LONTILL = gfaktemp.LONTILL + sumtidtemp.LONKOST.                     
         ASSIGN     
         gfaktemp.RES = gfaktemp.RES + sumtidtemp.RESKOSTDEC.            
      END.
      GET NEXT BRW_TID NO-LOCK.              
   END.   
   IF rundavar = TRUE THEN DO:
      ASSIGN
      gfaktemp.TOTALT = runda(gfaktemp.TOTALT)
      gfaktemp.RES = runda(gfaktemp.RES) 
      gfaktemp.ARBKOST = runda(gfaktemp.ARBKOST) 
      gfaktemp.LONTILL = runda(gfaktemp.LONTILL) 
      gfaktemp.MTRL = runda(gfaktemp.MTRL) 
      gfaktemp.OBELOPP = runda(gfaktemp.OBELOPP) 
      gfaktemp.OVRIG = runda(gfaktemp.OVRIG) 
      gfaktemp.TRAKT = runda(gfaktemp.TRAKT)       
      gfaktemp.KBELOPP = runda(gfaktemp.KBELOPP).
   END.                                              
   IF vartyp = 3 OR vartyp = 1 OR vartyp = 2 OR vartyp = 52 THEN DO:
      gfaktemp.TOTALT = gfaktemp.TOTALT + gfaktemp.RES + 
                        gfaktemp.ARBKOST + gfaktemp.LONTILL + gfaktemp.MTRL + 
                        gfaktemp.OBELOPP + gfaktemp.OVRIG + gfaktemp.TRAKT +       
                        gfaktemp.KBELOPP.
      IF rundavar = TRUE THEN DO:
         ASSIGN
         gfaktemp.TOTALT = runda(gfaktemp.TOTALT).
      END.                  
   END.
   ELSE DO:
      gfaktemp.TOTALT = gfaktemp.RES + 
                        gfaktemp.ARBKOST + gfaktemp.LONTILL + gfaktemp.MTRL + 
                        gfaktemp.OBELOPP + gfaktemp.OVRIG + gfaktemp.TRAKT + 
                        gfaktemp.KBELOPP.
      IF rundavar = TRUE THEN DO:
         ASSIGN
         gfaktemp.TOTALT = runda(gfaktemp.TOTALT).
      END.                  
   END.             
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
   {muswait.i}   
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE opentid_UI WINDOW-3 
PROCEDURE opentid_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   /*verkar inte användas*/
   {muswait.i}
   FIND FIRST gfaktemp WHERE gfaktemp.ORDNING = 3 NO-ERROR.
   IF kundregeltemp.TIMRGL = "TIDREDOVISNING" THEN DO:
      gfaktemp.ARBKOST = vararbkost.                    
   END.       
   IF kundregeltemp.OVERTIDRGL = "TIDREDOVISNING" THEN DO:
      gfaktemp.OBELOPP = 0.                               
   END. 
   IF kundregeltemp.TIMRGL = "KUNDENS EGNA" THEN DO:
      gfaktemp.ARBKOST = vararbkost.                    
   END.  
   IF kundregeltemp.OVERTIDRGL = "EGNA REGLER" THEN DO:
      gfaktemp.OBELOPP = 0.                               
   END.     
   IF kundregeltemp.TRAKTRGL = "TIDREDOVISNING" OR 
   kundregeltemp.TRAKTRGL = "EGET PRIS" THEN DO:         
      gfaktemp.TRAKT = vartrakost.                            
   END.
   IF kundregeltemp.LONRGL = "TIDREDOVISNING" OR 
   kundregeltemp.LONRGL = "ENDAST MILERSÄTTNING" THEN DO:
      gfaktemp.LONTILL = 0.                     
   END.  
   GET FIRST BRW_TID NO-LOCK.
   gfaktemp.RES = 0.
   DO WHILE AVAILABLE(sumtidtemp):            
      IF sumtidtemp.MED = TRUE THEN DO:         
         IF kundregeltemp.TIMRGL = "TIDREDOVISNING" THEN DO:
            gfaktemp.ARBKOST = gfaktemp.ARBKOST + sumtidtemp.BELOPP.                    
         END.       
         IF kundregeltemp.OVERTIDRGL = "TIDREDOVISNING" THEN DO:
            gfaktemp.OBELOPP = gfaktemp.OBELOPP + sumtidtemp.OBELOPP.                               
         END. 
         IF kundregeltemp.TIMRGL = "KUNDENS EGNA" THEN DO:
            gfaktemp.ARBKOST = gfaktemp.ARBKOST + sumtidtemp.BELOPP.                    
         END.  
         IF kundregeltemp.OVERTIDRGL = "EGNA REGLER" THEN DO:
            gfaktemp.OBELOPP = gfaktemp.OBELOPP + sumtidtemp.OBELOPP.                               
         END.         
         IF kundregeltemp.TRAKTRGL = "TIDREDOVISNING" OR 
         kundregeltemp.TRAKTRGL = "EGET PRIS" THEN DO:         
            gfaktemp.TRAKT = gfaktemp.TRAKT + sumtidtemp.TBELOPP.                            
         END.
         IF kundregeltemp.LONRGL = "TIDREDOVISNING" OR
         kundregeltemp.LONRGL = "ENDAST MILERSÄTTNING" THEN DO:
            gfaktemp.LONTILL = gfaktemp.LONTILL + sumtidtemp.LONKOST.                     
         END.          
         gfaktemp.RES = gfaktemp.RES + sumtidtemp.RESKOSTDEC.    
      END.       
      GET NEXT BRW_TID NO-LOCK.                 
   END.                
   IF rundavar = TRUE THEN DO:
      ASSIGN
      gfaktemp.TOTALT = runda(gfaktemp.TOTALT)
      gfaktemp.RES = runda(gfaktemp.RES) 
      gfaktemp.ARBKOST = runda(gfaktemp.ARBKOST) 
      gfaktemp.LONTILL = runda(gfaktemp.LONTILL) 
      gfaktemp.MTRL = runda(gfaktemp.MTRL) 
      gfaktemp.OBELOPP = runda(gfaktemp.OBELOPP) 
      gfaktemp.OVRIG = runda(gfaktemp.OVRIG) 
      gfaktemp.TRAKT = runda(gfaktemp.TRAKT)       
      gfaktemp.KBELOPP = runda(gfaktemp.KBELOPP).
   END.                               
   IF vartyp = 3 OR 
   vartyp = 1 OR vartyp = 2 OR vartyp = 52 THEN DO:
      gfaktemp.TOTALT = gfaktemp.TOTALT + 
                        gfaktemp.MTRL + gfaktemp.OVRIG + gfaktemp.ARBKOST + 
                        gfaktemp.OBELOPP + gfaktemp.TRAKT + gfaktemp.RES + 
                        gfaktemp.LONTILL + gfaktemp.KBELOPP.
   END.
   ELSE DO:
      gfaktemp.TOTALT = gfaktemp.MTRL + gfaktemp.OVRIG + gfaktemp.ARBKOST + 
                        gfaktemp.OBELOPP + gfaktemp.TRAKT + gfaktemp.RES + 
                        gfaktemp.LONTILL + gfaktemp.KBELOPP.
   END. 
   ENABLE BRW_TID WITH FRAME {&FRAME-NAME}.     
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
   {muswait.i}
   /*
   styrkfmtemp.STYRVAL = 5 kund
   styrkfmtemp.STYRVAL = 6 INTäkt
   styrkfmtemp.STYRVAL = 7 motpart
   styrkfmtemp.STYRVAL = 8 moms
   */
   FIND FIRST kundregeltemp WHERE kundregeltemp.FAKTNR = infakplannr 
   NO-LOCK NO-ERROR.
   
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
   {muswait.i}
   FIND FIRST kundbeftemp WHERE kundbeftemp.BEFATTNING = sumtidtemp.BEFATTNING NO-LOCK NO-ERROR.
   ASSIGN
   sumtidtemp.PERSBEF = sumtidtemp.PERSONALKOD + " " + kundbeftemp.VIBEFATTNING
   sumtidtemp.VIBEFATTNING = kundbeftemp.VIBEFATTNING.
   IF AVAILABLE kundovertemp THEN DO:
      ASSIGN
      sumtidtemp.VIOBEFATTNING = kundovertemp.OTEXT
      sumtidtemp.OTEXTID = kundovertemp.OTEXTID. 
      IF kundovertemp.SLUT >= sumtidtemp.SLUT THEN DO:         
         /*belopp och timmar = 0 ????*/
         ASSIGN         
         sumtidtemp.BELOPP = 0                               
         sumtidtemp.TIMMAR = 0
         sumtidtemp.OBELOPP = 0                               
         sumtidtemp.OTIMMAR = klockan100(sumtidtemp.SLUT) - klockan100(sumtidtemp.START).                  
         ASSIGN
         sumtidtemp.OPRIS = kundovertemp.PRISA
         sumtidtemp.OBELOPP = (klockan100(sumtidtemp.SLUT) - klockan100(sumtidtemp.START)) * kundovertemp.PRISA.                                                                      
      END. 
      ELSE DO:         
         CREATE extrasum.
         BUFFER-COPY sumtidtemp TO extrasum.
         ASSIGN         
         extrasum.TIMMAR = 0
         extrasum.BELOPP = 0         
         extrasum.START = kundovertemp.SLUT
         extrasum.SLUT = sumtidtemp.SLUT
         extrasum.GSTART = kundovertemp.SLUT
         extrasum.GSLUT = sumtidtemp.SLUT         
         extrasum.OBELOPP = 0
         extrasum.OTIMMAR = 0    
         extrasum.OANT1 = sumtidtemp.OANT1 - sumtidtemp.OTIMMAR.
         IF extrasum.OANT1 < 0 THEN extrasum.OANT1 = 0.
         ASSIGN         
         sumtidtemp.OBELOPP = 0             
         sumtidtemp.SLUT = kundovertemp.SLUT
         sumtidtemp.BELOPP = 0                               
         sumtidtemp.TIMMAR = 0
         sumtidtemp.OBELOPP = 0.                                        
         sumtidtemp.OTIMMAR = 
         klockan100(kundovertemp.SLUT) - klockan100(sumtidtemp.START).   
         ASSIGN
         sumtidtemp.OPRIS = kundovertemp.PRISA
         sumtidtemp.OBELOPP = sumtidtemp.OTIMMAR * kundovertemp.PRISA.                        
      END.  
   END.   
   ELSE DO:                   
      IF vareqdag = 1 THEN DO:
         FIND FIRST kundovertemp WHERE kundovertemp.FAKTNR = vfaktplantemp.FAKTNR AND
         kundovertemp.BEFATTNING = sumtidtemp.BEFATTNING AND kundovertemp.EQDAG = 8 AND
         kundovertemp.START < sumtidtemp.SLUT AND
         kundovertemp.SLUT >= sumtidtemp.SLUT 
         NO-LOCK NO-ERROR.
      END.  
      ELSE IF vareqdag = 7 THEN DO:
         FIND FIRST kundovertemp WHERE kundovertemp.FAKTNR = vfaktplantemp.FAKTNR AND
         kundovertemp.BEFATTNING = sumtidtemp.BEFATTNING AND kundovertemp.EQDAG = 7 AND
         kundovertemp.START < sumtidtemp.SLUT AND
         kundovertemp.SLUT >= sumtidtemp.SLUT 
         NO-LOCK NO-ERROR.
      END.
      ELSE DO:
         FIND FIRST kundovertemp WHERE kundovertemp.FAKTNR = vfaktplantemp.FAKTNR AND
         kundovertemp.BEFATTNING = sumtidtemp.BEFATTNING AND kundovertemp.EQDAG = 2 AND
         kundovertemp.START < sumtidtemp.SLUT AND
         kundovertemp.SLUT >= sumtidtemp.SLUT 
         NO-LOCK NO-ERROR.
      END.
      IF NOT AVAILABLE kundovertemp THEN DO: 
         IF (klockan100(sumtidtemp.SLUT) - klockan100(sumtidtemp.START)) <= sumtidtemp.LUNCH THEN sumtidtemp.LUNCH = 0.
         ASSIGN
         sumtidtemp.OBELOPP = 0                               
         sumtidtemp.OTIMMAR = 0.
         sumtidtemp.TIMMAR = (klockan100(sumtidtemp.SLUT) - klockan100(sumtidtemp.START)) - sumtidtemp.LUNCH.
         sumtidtemp.BELOPP = sumtidtemp.TIMMAR * sumtidtemp.PRISA.
      END.
      ELSE DO:
         ASSIGN
         sumtidtemp.VIOBEFATTNING = kundovertemp.OTEXT
         sumtidtemp.OTEXTID = kundovertemp.OTEXTID.
         IF kundovertemp.START <= sumtidtemp.START THEN DO:            
            ASSIGN         
            sumtidtemp.BELOPP = 0                               
            sumtidtemp.TIMMAR = 0
            sumtidtemp.OBELOPP = 0                               
            sumtidtemp.OTIMMAR = klockan100(sumtidtemp.SLUT) - klockan100(sumtidtemp.START).         
            ASSIGN
            sumtidtemp.OPRIS = kundovertemp.PRISA
            sumtidtemp.OBELOPP = (klockan100(sumtidtemp.SLUT) - klockan100(sumtidtemp.START)) * kundovertemp.PRISA.                                                                         
         END.             
         ELSE DO:                             
            CREATE extrasum.
            BUFFER-COPY sumtidtemp TO extrasum. 
            ASSIGN                
            extrasum.TIMMAR = 0
            extrasum.BELOPP = 0         
            extrasum.START = sumtidtemp.START
            extrasum.SLUT = kundovertemp.START
            extrasum.GSTART = sumtidtemp.START
            extrasum.GSLUT = kundovertemp.START         
            extrasum.OBELOPP = 0
            extrasum.OTIMMAR = 0                
            extrasum.OANT1 = sumtidtemp.OANT1 - sumtidtemp.OTIMMAR.
            IF extrasum.OANT1 < 0 THEN extrasum.OANT1 = 0.
            ASSIGN                     
            sumtidtemp.START = kundovertemp.START    
            sumtidtemp.BELOPP = 0                               
            sumtidtemp.TIMMAR = 0
            sumtidtemp.OBELOPP = 0.                                        
            sumtidtemp.OTIMMAR = 
            klockan100(sumtidtemp.SLUT) - klockan100(kundovertemp.START).                                                               
            ASSIGN
            sumtidtemp.OPRIS = kundovertemp.PRISA
            sumtidtemp.OBELOPP = sumtidtemp.OTIMMAR * kundovertemp.PRISA.                                        
         END.  
      END. 
   END.
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
   {muswait.i}
   IF vareqdag = 1 THEN DO:  
      FIND FIRST kundovertemp WHERE kundovertemp.FAKTNR = vfaktplantemp.FAKTNR AND
      kundovertemp.BEFATTNING = sumtidtemp.BEFATTNING AND kundovertemp.EQDAG = 8 AND
      kundovertemp.START LE sumtidtemp.START AND
      kundovertemp.SLUT > sumtidtemp.START 
      NO-LOCK NO-ERROR.
      RUN overberbyt2_UI.      
   END.   
   ELSE IF vareqdag = 7 THEN DO:
      FIND FIRST kundovertemp WHERE kundovertemp.FAKTNR = vfaktplantemp.FAKTNR AND
      kundovertemp.BEFATTNING = sumtidtemp.BEFATTNING AND kundovertemp.EQDAG = 7 AND
      kundovertemp.START LE sumtidtemp.START AND
      kundovertemp.SLUT > sumtidtemp.START 
      NO-LOCK NO-ERROR.
      RUN overberbyt2_UI.            
   END.   
   ELSE DO:            
      FIND FIRST kundovertemp WHERE kundovertemp.FAKTNR = vfaktplantemp.FAKTNR AND
      kundovertemp.BEFATTNING = sumtidtemp.BEFATTNING AND kundovertemp.EQDAG = 2 AND
      kundovertemp.START LE sumtidtemp.START AND
      kundovertemp.SLUT > sumtidtemp.START 
      NO-LOCK NO-ERROR.
      RUN overberbyt2_UI.         
   END. 
     
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
   {muswait.i}         
   IF RAD_VISA = 4 THEN RUN visa_UI (INPUT FALSE).
   ELSE IF RAD_VISA = 6 THEN RUN visa_UI (INPUT FALSE).
   ELSE RUN visa_UI (INPUT TRUE).   
   IF vartyp = 1 OR vartyp = 2 OR vartyp = 3 OR vartyp = 52 OR vartyp = 6 OR vartyp = 7 THEN DO:
      ASSIGN
      BTN_BACK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BTN_OVER:HIDDEN = TRUE
      BRW_K4:HIDDEN = TRUE
      BRW_KOSTMOMS:HIDDEN = TRUE
      TOG_DIFF:HIDDEN = TRUE.      
   END.
   ELSE IF RAD_VISA NE 2 THEN DO:
      ASSIGN
      BTN_BACK:HIDDEN = TRUE
      BTN_OVER:HIDDEN = TRUE
      BRW_K4:HIDDEN = TRUE
      BRW_KOSTMOMS:HIDDEN = TRUE
      TOG_DIFF:HIDDEN = TRUE.      
   END.
   ELSE DO:
      TOG_DIFF:HIDDEN = FALSE.
      DISPLAY TOG_DIFF WITH FRAME {&FRAME-NAME}.
      APPLY "VALUE-CHANGED" TO TOG_DIFF.            
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spara_UI WINDOW-3 
PROCEDURE spara_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER skarp AS INTEGER NO-UNDO.
   {muswait.i}
   IF skarp = 0 THEN vfaktplantemp.FDELNR = FILL-IN_FDELNR.      
   FIND FIRST gfaktemp WHERE gfaktemp.ORDNING = 3 NO-ERROR.
   RUN hamtfaktku_UI IN faktupphmth 
   (INPUT infakplannr,INPUT FILL-IN_FDELNR,OUTPUT TABLE efaktkredtemp).
   FIND FIRST efaktkredtemp WHERE efaktkredtemp.FAKTNR = infakplannr AND
   efaktkredtemp.FDELNR = FILL-IN_FDELNR NO-LOCK NO-ERROR.
   ASSIGN
   efaktkredtemp.SPARAD = TRUE
   efaktkredtemp.VKREDIT = skarp
   efaktkredtemp.TOTPRIS = gfaktemp.TOTALT /*- FAKTKRED.AVGAR*/     
   efaktkredtemp.RESKOSTDEC = gfaktemp.RES
   efaktkredtemp.BELOPP = gfaktemp.ARBKOST         
   efaktkredtemp.OBELOPP = gfaktemp.OBELOPP             
   efaktkredtemp.TBELOPP = gfaktemp.TRAKT
   efaktkredtemp.LONKOST = gfaktemp.LONTILL
   efaktkredtemp.MTRLKOST = gfaktemp.MTRL
   efaktkredtemp.OVRKOST = gfaktemp.OVRIG
   efaktkredtemp.KOSTBELOPP = gfaktemp.KBELOPP.      
   IF rundavar = TRUE THEN DO:
      ASSIGN
      efaktkredtemp.TOTPRIS = runda(efaktkredtemp.TOTPRIS)     
      efaktkredtemp.RESKOSTDEC = runda(efaktkredtemp.RESKOSTDEC)
      efaktkredtemp.BELOPP = runda(efaktkredtemp.BELOPP)         
      efaktkredtemp.OBELOPP = runda(efaktkredtemp.OBELOPP)             
      efaktkredtemp.TBELOPP = runda(efaktkredtemp.TBELOPP)
      efaktkredtemp.LONKOST = runda(efaktkredtemp.LONKOST)
      efaktkredtemp.MTRLKOST = runda(efaktkredtemp.MTRLKOST)
      efaktkredtemp.OVRKOST = runda(efaktkredtemp.OVRKOST)
      efaktkredtemp.KOSTBELOPP = runda(efaktkredtemp.KOSTBELOPP).                   
   END.      
   RUN sparfaktk_UI IN faktupphmth (INPUT infakplannr,INPUT FILL-IN_FDELNR,
   INPUT TABLE efaktkredtemp,INPUT TABLE faktstarttemp,INPUT TABLE faktupparbtemp).
   IF Guru.Konstanter:appcon THEN DO:
      RUN FASPARAKA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT infakplannr, INPUT FILL-IN_FDELNR,INPUT skarpvar,INPUT vartyp, 
       INPUT hamttid, INPUT hamtkost, INPUT kollvecko, 
       INPUT TABLE kosttemp,INPUT TABLE sumtidtemp).
   END.
   ELSE DO:
      RUN FASPARAKA.P 
      (INPUT infakplannr, INPUT FILL-IN_FDELNR,INPUT skarpvar,INPUT vartyp, 
       INPUT hamttid, INPUT hamtkost, INPUT kollvecko, 
       INPUT TABLE kosttemp,INPUT TABLE sumtidtemp).
   END.
   {musarrow.i}
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
   {muswait.i}   
   IF Guru.Konstanter:appcon THEN DO:
      RUN STIDKAPPA.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT infakplannr,INPUT kreditnrvar,OUTPUT TABLE sumtidtemp).
   END.
   ELSE DO:
      RUN STIDKAPPA.P 
      (INPUT infakplannr,INPUT kreditnrvar,OUTPUT TABLE sumtidtemp).
   END.
   
   
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
   IF {&WINDOW-NAME}:WIDTH-CHARS = {&WINDOW-NAME}:MAX-WIDTH-CHARS THEN DO:
      BRW_TID:FONT IN FRAME {&FRAME-NAME} = 0.
      /*BRW_TID:WIDTH-CHARS = 125.*/
      ASSIGN         
      sumtidtemp.DATUM:WIDTH-CHARS IN BROWSE BRW_TID = 8
      sumtidtemp.START:WIDTH-CHARS = 6
      sumtidtemp.SLUT:WIDTH-CHARS = 6
      sumtidtemp.TIMMAR:WIDTH-CHARS = 6
      sumtidtemp.BELOPP:WIDTH-CHARS = 7
      sumtidtemp.OTIMMAR:WIDTH-CHARS = 6
      sumtidtemp.OBELOPP:WIDTH-CHARS = 7
      sumtidtemp.VITRAKT:WIDTH-CHARS = 4
      sumtidtemp.TRAKTANTAL:WIDTH-CHARS = 5
      sumtidtemp.TBELOPP:WIDTH-CHARS = 6
      sumtidtemp.VILART:WIDTH-CHARS = 4
      sumtidtemp.LONTILLANTAL:WIDTH-CHARS = 6
      sumtidtemp.LONKOST:WIDTH-CHARS = 8
      sumtidtemp.RESTIM:WIDTH-CHARS = 5
      sumtidtemp.RESKOSTDEC:WIDTH-CHARS = 6
      sumtidtemp.AONR:WIDTH-CHARS = 6
      sumtidtemp.DELNR:WIDTH-CHARS = 3
      sumtidtemp.MED:WIDTH-CHARS = 3.
      /*
      sumtidtemp.PERSONALKOD + " " + sumtidtemp.VIBEFATTNING:WIDTH-CHARS = 17.
      */
   END.
   ELSE DO:
      /*BRW_TID:WIDTH-CHARS = 95.25.*/
      BRW_TID:FONT = 11.
      ASSIGN         
      sumtidtemp.DATUM:WIDTH-CHARS IN BROWSE BRW_TID = 5.25
      sumtidtemp.START:WIDTH-CHARS        = 4.13
      sumtidtemp.SLUT:WIDTH-CHARS         = 4.13
      sumtidtemp.TIMMAR:WIDTH-CHARS       = 5.13
      sumtidtemp.BELOPP:WIDTH-CHARS       = 5
      sumtidtemp.OTIMMAR:WIDTH-CHARS      = 4.88
      sumtidtemp.OBELOPP:WIDTH-CHARS      = 5
      sumtidtemp.VITRAKT:WIDTH-CHARS      = 2.75
      sumtidtemp.TRAKTANTAL:WIDTH-CHARS   = 3.63
      sumtidtemp.TBELOPP:WIDTH-CHARS      = 3.88
      sumtidtemp.VILART:WIDTH-CHARS       = 2.75
      sumtidtemp.LONTILLANTAL:WIDTH-CHARS = 4.13
      sumtidtemp.LONKOST:WIDTH-CHARS      = 5   
      sumtidtemp.RESTIM:WIDTH-CHARS       = 3.13
      sumtidtemp.RESKOSTDEC:WIDTH-CHARS   = 4.25
      sumtidtemp.AONR:WIDTH-CHARS         = 4.38
      sumtidtemp.DELNR:WIDTH-CHARS        = 2.25
      sumtidtemp.MED:WIDTH-CHARS          = 2.75      .
      /*
      sumtidtemp.PERSONALKOD + " " + sumtidtemp.VIBEFATTNING:WIDTH-CHARS = 17.
      */
   END.
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
   IF sumtidtemp.TIMMAR = INPUT BROWSE BRW_TID sumtidtemp.TIMMAR THEN musz = musz.
   ELSE DO:
      IF TOG_MOMS = TRUE THEN DO:
         RUN kontokoll_UI.
      END.
      ELSE DO:
         DEFINE VARIABLE gamla AS DECIMAL NO-UNDO.
         gamla = sumtidtemp.BELOPP.
         sumtidtemp.TIMMAR = INPUT BROWSE BRW_TID sumtidtemp.TIMMAR.   
         DISPLAY sumtidtemp.TIMMAR WITH BROWSE BRW_TID.
         IF sumtidtemp.OTIMMAR = 0 AND sumtidtemp.RESTIM = 0 THEN DO:      
            sumtidtemp.BELOPP = sumtidtemp.TIMMAR * sumtidtemp.PRISA.
            DISPLAY sumtidtemp.BELOPP sumtidtemp.TIMMAR WITH BROWSE BRW_TID.
            RUN findg3_UI (INPUT 0, INPUT gamla, INPUT sumtidtemp.BELOPP, INPUT 0,INPUT 0, 
                           INPUT 0, INPUT 0, INPUT 0, INPUT 0, sumtidtemp.MED).         
         END.
         ELSE DO: 
            sumtidtemp.TIMMAR = 0.
            DISPLAY sumtidtemp.TIMMAR WITH BROWSE BRW_TID.
         END.
      END.   
      DISPLAY sumtidtemp.TIMMAR WITH BROWSE BRW_TID.
   END.   
   {musarrow.i}
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
   {muswait.i}  
   OPEN QUERY faktbefq FOR EACH kundbeftemp WHERE kundbeftemp.FAKTNR = infakplannr
   NO-LOCK.
   GET FIRST faktbefq NO-LOCK.
   DO WHILE AVAILABLE(kundbeftemp):         
      FOR EACH sumtidtemp WHERE sumtidtemp.BEFATTNING = kundbeftemp.BEFATTNING USE-INDEX BEF.         
         ASSIGN
         sumtidtemp.PERSBEF = sumtidtemp.PERSONALKOD + " " + kundbeftemp.VIBEFATTNING
         sumtidtemp.VIBEFATTNING = kundbeftemp.VIBEFATTNING
         sumtidtemp.RESKOSTDEC = sumtidtemp.RESTIM * kundbeftemp.PRISRES
         sumtidtemp.BELOPP = sumtidtemp.TIMMAR * kundbeftemp.PRISA
         sumtidtemp.RESPRIS = kundbeftemp.PRISRES
         sumtidtemp.PRISA = kundbeftemp.PRISA.
      END.
      GET NEXT faktbefq NO-LOCK.    
   END. 
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
   {muswait.i}
   IF TOG_DIFF = TRUE THEN DO:
      IF BRW_KOSTMOMS:VISIBLE IN FRAME {&FRAME-NAME} = TRUE THEN musz = musz.
      ELSE DO:
         ASSIGN
         BRW_KOSTMOMS:HIDDEN = FALSE
         BRW_K4:HIDDEN = FALSE
         BRW_KOST:HIDDEN = TRUE.
         IF TOG_MOMS = TRUE THEN DO:
            RUN kontokoll_UI.      
            ASSIGN      
            BTN_BACK:HIDDEN = TRUE
            BTN_OVER:HIDDEN = TRUE.
         END.
         ELSE DO:
            ASSIGN      
            BTN_BACK:HIDDEN = FALSE
            BTN_OVER:HIDDEN = FALSE.
            ENABLE BTN_OVER BTN_BACK BRW_KOSTMOMS BRW_K4 WITH FRAME {&FRAME-NAME}.
         END.
         ENABLE BRW_KOSTMOMS BRW_K4 WITH FRAME {&FRAME-NAME}.     
         OPEN QUERY BRW_KOSTMOMS FOR EACH kosttemp.      
      END.    
   END.
   ELSE DO:
      IF BRW_KOST:VISIBLE = TRUE THEN musz = musz.
      ELSE DO:
         ASSIGN
         BTN_BACK:HIDDEN = TRUE
         BTN_OVER:HIDDEN = TRUE
         BRW_K4:HIDDEN = TRUE
         BRW_KOSTMOMS:HIDDEN = TRUE
         BRW_KOST:HIDDEN = FALSE.
         RUN refreshbrw_UI IN brwproc[7].
      END.      
   END.
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
   {muswait.i}
   DEFINE INPUT PARAMETER fraga AS LOGICAL NO-UNDO. 
   IF vartyp = 1 OR vartyp = 2 OR vartyp = 3 OR vartyp = 52 THEN DO:      
      IF RAD_VISA = 1 THEN DO:                  
      END.
      ELSE IF RAD_VISA = 2 THEN DO:                                                       
      END.
      ELSE IF RAD_VISA = 4 OR RAD_VISA = 6 THEN DO:                          
      END.         
   END.
   IF RAD_VISA = 5 THEN DO:
      EMPTY TEMP-TABLE sumpers NO-ERROR. 
      FOR EACH sumtidtemp WHERE sumtidtemp.MED = TRUE  BREAK 
      BY sumtidtemp.PRISA BY sumtidtemp.AONR BY sumtidtemp.DELNR:         
         ACCUMULATE 
         sumtidtemp.BELOPP (TOTAL BY sumtidtemp.PRISA BY sumtidtemp.AONR BY sumtidtemp.DELNR).
         ACCUMULATE 
         sumtidtemp.TIMMAR (TOTAL BY sumtidtemp.PRISA BY sumtidtemp.AONR BY sumtidtemp.DELNR).
         IF LAST-OF(sumtidtemp.DELNR) THEN DO:
            IF (ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.TIMMAR) > 0 THEN DO:
               CREATE sumpers.
               ASSIGN                
               sumpers.VIBEFATTNING = "Arbetskostnad"            
               sumpers.ORDNING = 1
               sumpers.AONR = sumtidtemp.AONR
               sumpers.DELNR = sumtidtemp.DELNR               
               sumpers.OTIMMAR = sumtidtemp.PRISA 
               sumpers.TIMMAR = 
               (ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.TIMMAR)
               sumpers.BELOPP = 
               (ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.BELOPP).                              
            END.
         END.
      END.
      FOR EACH sumtidtemp WHERE sumtidtemp.MED = TRUE  BREAK 
      BY sumtidtemp.OPRIS BY sumtidtemp.AONR BY sumtidtemp.DELNR:         
         ACCUMULATE 
         sumtidtemp.OBELOPP (TOTAL BY sumtidtemp.OPRIS BY sumtidtemp.AONR BY sumtidtemp.DELNR).
         ACCUMULATE 
         sumtidtemp.OTIMMAR (TOTAL BY sumtidtemp.OPRIS BY sumtidtemp.AONR BY sumtidtemp.DELNR).
         IF LAST-OF(sumtidtemp.DELNR) THEN DO:
            IF (ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.OBELOPP) > 0 THEN DO:
               CREATE sumpers.
               ASSIGN                
               sumpers.VIBEFATTNING = "Övertidskostnad"            
               sumpers.ORDNING = 2
               sumpers.AONR = sumtidtemp.AONR
               sumpers.DELNR = sumtidtemp.DELNR               
               sumpers.OTIMMAR = sumtidtemp.OPRIS 
               sumpers.TIMMAR = 
               (ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.OTIMMAR)
               sumpers.BELOPP = 
               (ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.OBELOPP).                              
            END.
         END.
      END.
      FOR EACH sumtidtemp WHERE sumtidtemp.MED = TRUE  BREAK 
      BY sumtidtemp.RESPRIS BY sumtidtemp.AONR BY sumtidtemp.DELNR:         
         ACCUMULATE 
         sumtidtemp.RESKOSTDEC (TOTAL BY sumtidtemp.RESPRIS BY sumtidtemp.AONR BY sumtidtemp.DELNR).
         ACCUMULATE 
         sumtidtemp.RESTIM (TOTAL BY sumtidtemp.RESPRIS BY sumtidtemp.AONR BY sumtidtemp.DELNR).
         IF LAST-OF(sumtidtemp.DELNR) THEN DO:
            IF (ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.RESKOSTDEC) > 0 THEN DO:
               CREATE sumpers.
               ASSIGN                
               sumpers.VIBEFATTNING = "Restidskostnad"            
               sumpers.ORDNING = 3
               sumpers.AONR = sumtidtemp.AONR
               sumpers.DELNR = sumtidtemp.DELNR               
               sumpers.OTIMMAR = sumtidtemp.RESPRIS 
               sumpers.TIMMAR = 
               (ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.RESTIM)
               sumpers.BELOPP = 
               (ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.RESKOSTDEC).                              
            END.
         END.
      END.
      FOR EACH sumtidtemp WHERE sumtidtemp.MED = TRUE  BREAK 
      BY sumtidtemp.TRAKTKOD BY sumtidtemp.AONR BY sumtidtemp.DELNR:         
         ACCUMULATE 
         sumtidtemp.TBELOPP (TOTAL BY sumtidtemp.TRAKTKOD BY sumtidtemp.AONR BY sumtidtemp.DELNR).
         ACCUMULATE 
         sumtidtemp.TRAKTANTAL (TOTAL BY sumtidtemp.TRAKTKOD BY sumtidtemp.AONR BY sumtidtemp.DELNR).
         IF LAST-OF(sumtidtemp.DELNR) THEN DO:
            IF (ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.TBELOPP) > 0 THEN DO:
               CREATE sumpers.
               ASSIGN                
               sumpers.VIBEFATTNING = "Trakt.kostnad " + sumtidtemp.VITRAKT            
               sumpers.ORDNING = 4
               sumpers.AONR = sumtidtemp.AONR
               sumpers.DELNR = sumtidtemp.DELNR                                  
               sumpers.TIMMAR = 
               (ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.TRAKTANTAL)
               sumpers.BELOPP =                
               (ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.TBELOPP).
               IF sumpers.TIMMAR > 0 THEN 
               sumpers.OTIMMAR = sumpers.BELOPP / sumpers.TIMMAR.                  
            END.
         END.
      END.         
      FOR EACH sumtidtemp WHERE sumtidtemp.MED = TRUE  BREAK 
      BY sumtidtemp.LONTILLAGG BY sumtidtemp.AONR BY sumtidtemp.DELNR:         
         ACCUMULATE 
         sumtidtemp.LONKOST (TOTAL BY sumtidtemp.LONTILLAGG BY sumtidtemp.AONR BY sumtidtemp.DELNR).
         ACCUMULATE 
         sumtidtemp.LONTILLANTAL (TOTAL BY sumtidtemp.LONTILLAGG BY sumtidtemp.AONR BY sumtidtemp.DELNR).
         IF LAST-OF(sumtidtemp.DELNR) THEN DO:
            IF (ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.LONKOST) > 0 THEN DO:
               CREATE sumpers.
               ASSIGN                
               sumpers.VIBEFATTNING = "Lart.kostnad " + sumtidtemp.VILART            
               sumpers.ORDNING = 5
               sumpers.AONR = sumtidtemp.AONR
               sumpers.DELNR = sumtidtemp.DELNR                                  
               sumpers.TIMMAR = 
               (ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.LONTILLANTAL)
               sumpers.BELOPP = 
               (ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.LONKOST).
               IF sumpers.TIMMAR > 0 THEN 
               sumpers.OTIMMAR = sumpers.BELOPP / sumpers.TIMMAR.                  
            END.
         END.
      END.
      
      OPEN QUERY BRW_TIDT FOR EACH sumpers NO-LOCK BY sumpers.PERSONALKOD 
      BY sumpers.AONR BY sumpers.DELNR.         
      FOR EACH kosttemp WHERE kosttemp.MED = TRUE  BREAK 
      BY kosttemp.AONR BY kosttemp.DELNR:         
         ACCUMULATE 
         kosttemp.MASKKOST (TOTAL BY kosttemp.AONR BY kosttemp.DELNR).
         ACCUMULATE 
         kosttemp.FRTJPAKR (TOTAL BY kosttemp.AONR BY kosttemp.DELNR).
         ACCUMULATE 
         kosttemp.MTRL (TOTAL BY kosttemp.AONR BY kosttemp.DELNR). 
         ACCUMULATE 
         kosttemp.MTRLPAKR (TOTAL BY kosttemp.AONR BY kosttemp.DELNR).
         ACCUMULATE 
         kosttemp.OVRKR (TOTAL BY kosttemp.AONR BY kosttemp.DELNR).
         ACCUMULATE 
         kosttemp.PERSKOST (TOTAL BY kosttemp.AONR BY kosttemp.DELNR).
         ACCUMULATE 
         kosttemp.TRAKTKOST (TOTAL BY kosttemp.AONR BY kosttemp.DELNR).         
         IF LAST-OF(kosttemp.DELNR) THEN DO:
            IF (ACCUM TOTAL BY kosttemp.DELNR kosttemp.PERSKOST) > 0 THEN DO:
               CREATE sumpers.
               ASSIGN                
               sumpers.VIBEFATTNING = "Konterad personalkostnad."           
               sumpers.ORDNING = 6
               sumpers.AONR = kosttemp.AONR
               sumpers.DELNR = kosttemp.DELNR                                  
               sumpers.OTIMMAR = 0 
               sumpers.TIMMAR = 1
               sumpers.BELOPP = (ACCUM TOTAL BY kosttemp.DELNR kosttemp.PERSKOST).                 
            END.
            IF (ACCUM TOTAL BY kosttemp.DELNR kosttemp.TRAKTKOST) > 0 THEN DO:
               CREATE sumpers.
               ASSIGN                
               sumpers.VIBEFATTNING = "Konterad trakt.kostnad."           
               sumpers.ORDNING = 7
               sumpers.AONR = kosttemp.AONR
               sumpers.DELNR = kosttemp.DELNR                                  
               sumpers.OTIMMAR = 0 
               sumpers.TIMMAR = 1
               sumpers.BELOPP = (ACCUM TOTAL BY kosttemp.DELNR kosttemp.TRAKTKOST).                 
            END.
            IF (ACCUM TOTAL BY kosttemp.DELNR kosttemp.MASKKOST) +  
            (ACCUM TOTAL BY kosttemp.DELNR kosttemp.FRTJPAKR) > 0 THEN DO:
               CREATE sumpers.
               ASSIGN                
               sumpers.VIBEFATTNING = "Främandetj."           
               sumpers.ORDNING = 8
               sumpers.AONR = kosttemp.AONR
               sumpers.DELNR = kosttemp.DELNR                                  
               sumpers.OTIMMAR = 0 
               sumpers.TIMMAR = 1
               sumpers.BELOPP = (ACCUM TOTAL BY kosttemp.DELNR kosttemp.MASKKOST) +  
                                (ACCUM TOTAL BY kosttemp.DELNR kosttemp.FRTJPAKR). 
                 
            END.
            IF (ACCUM TOTAL BY kosttemp.DELNR kosttemp.MTRL) +  
               (ACCUM TOTAL BY kosttemp.DELNR kosttemp.MTRLPAKR) > 0 THEN DO:
               CREATE sumpers.
               ASSIGN                
               sumpers.VIBEFATTNING = "Materielkostnad."           
               sumpers.ORDNING = 9
               sumpers.AONR = kosttemp.AONR
               sumpers.DELNR = kosttemp.DELNR                                  
               sumpers.OTIMMAR = 0 
               sumpers.TIMMAR = 1
               sumpers.BELOPP = (ACCUM TOTAL BY kosttemp.DELNR kosttemp.MTRL) +  
                                (ACCUM TOTAL BY kosttemp.DELNR kosttemp.MTRLPAKR). 
                
            END.
            IF (ACCUM TOTAL BY kosttemp.DELNR kosttemp.OVRKR) > 0 THEN DO:
               CREATE sumpers.
               ASSIGN                
               sumpers.VIBEFATTNING = "Övrig kostnad."           
               sumpers.ORDNING = 10
               sumpers.AONR = kosttemp.AONR
               sumpers.DELNR = kosttemp.DELNR                                  
               sumpers.OTIMMAR = 0 
               sumpers.TIMMAR = 1
               sumpers.BELOPP = (ACCUM TOTAL BY kosttemp.DELNR kosttemp.OVRKR).                   
            END.               
         END.
      END.
      /*FAKTFORE*/
      IF Guru.Konstanter:varforetypval[3] >= 1 THEN DO:
         FOR EACH faktfriatemp WHERE faktfriatemp.FAKTNR = infakplannr AND
         faktfriatemp.FDELNR = efaktkredtemp.FDELNR AND faktfriatemp.FAKTURERAD = TRUE AND 
         faktfriatemp.FAKTTEXT NE "Korigering mot takpris"
         NO-LOCK BREAK 
         BY faktfriatemp.AONR BY faktfriatemp.DELNR:         
            ACCUMULATE 
            faktfriatemp.TOTALT (TOTAL BY faktfriatemp.AONR BY faktfriatemp.DELNR).            
            IF LAST-OF(faktfriatemp.DELNR) THEN DO:
               IF (ACCUM TOTAL BY faktfriatemp.DELNR faktfriatemp.TOTALT) > 0 THEN DO:              
                  CREATE sumpers.
                  ASSIGN                
                  sumpers.VIBEFATTNING = "Fri komplettering"           
                  sumpers.ORDNING = 11
                  sumpers.AONR = faktfriatemp.AONR
                  sumpers.DELNR = faktfriatemp.DELNR                                  
                  sumpers.OTIMMAR = 0 
                  sumpers.TIMMAR = 1
                  sumpers.BELOPP = (ACCUM TOTAL BY faktfriatemp.DELNR faktfriatemp.TOTALT).                  
               END.                              
            END.
         END.
      END.
      IF vartyp = 1 OR vartyp = 2 OR vartyp = 3 THEN DO:
         IF vartyp = 3 THEN musz = musz.
         ELSE DO:
            FOR EACH faktstarttemp WHERE faktstarttemp.FAKTNR = infakplannr AND
            faktstarttemp.FDELNR = efaktkredtemp.FDELNR AND faktstarttemp.FAKTURERAD = TRUE 
            NO-LOCK:
               ACCUMULATE faktstarttemp.BELOPP (TOTAL). 
            END.
            IF (ACCUM TOTAL faktstarttemp.BELOPP) NE 0 THEN DO:
               CREATE sumpers.
               ASSIGN                
               sumpers.VIBEFATTNING = "Fasta poster"           
               sumpers.ORDNING = 12                               
               sumpers.BELOPP = (ACCUM TOTAL faktstarttemp.BELOPP).
            END.    
         END.
      END. 
      
      IF vartyp = 52 THEN DO:
         FOR EACH faktupparbtemp WHERE faktupparbtemp.FAKTNR = infakplannr AND
         faktupparbtemp.FDELNR = efaktkredtemp.FDELNR AND faktupparbtemp.FAKTURERAD = TRUE 
         NO-LOCK:
            ACCUMULATE faktupparbtemp.FAKTBELOPP (TOTAL). 
         END.
         IF (ACCUM TOTAL faktupparbtemp.FAKTBELOPP) NE 0 THEN DO:
            CREATE sumpers.
            ASSIGN                
            sumpers.VIBEFATTNING = "Upparbetde kostnader"           
            sumpers.ORDNING = 12                               
            sumpers.BELOPP = (ACCUM TOTAL faktupparbtemp.FAKTBELOPP).      
         END.
      END.
      
      OPEN QUERY BRW_ALLT FOR EACH sumpers NO-LOCK  
      BY sumpers.AONR BY sumpers.DELNR BY sumpers.ORDNING.   
      ENABLE BRW_ALLT WITH FRAME {&FRAME-NAME}.  
   END.
   ASSIGN
   BRW_UPP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BRW_PLAN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   BRW_KOST:HIDDEN IN FRAME {&FRAME-NAME} = TRUE  
   BRW_TID:HIDDEN IN FRAME {&FRAME-NAME} = TRUE   
   BRW_ALLT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.   
   RUN gomafri_UI (INPUT TRUE).
   IF RAD_VISA = 1 THEN DO:
      BRW_TID:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.      
   END.    
   ELSE IF RAD_VISA = 2 THEN DO:                   
      ASSIGN
      BRW_KOST:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.      
   END.
   ELSE IF RAD_VISA = 3 THEN DO:                  
      RUN gomafri_UI (INPUT FALSE).      
   END.   
   ELSE IF RAD_VISA = 4 THEN DO:                  
      ASSIGN      
      BRW_PLAN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.                            
   END.
   ELSE IF RAD_VISA = 5 THEN DO:                        
      BRW_ALLT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.            
   END.
   ELSE IF RAD_VISA = 6 THEN DO:                        
      BRW_UPP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.            
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

