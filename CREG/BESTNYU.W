&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-3


/* Temp-Table and Buffer definitions                                    */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-3 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/05/04 -  1:19 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER vem AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
                              
                              


{ORGPRIS.I}
{KUNDTYP.I}
{EXTRATAB.I}  
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
{EXTRADATA.I}
&Scoped-define NEW 
&Scoped-define SHARED 

{BESTKUNDALLT.I}
&Scoped-define SHARED SHARED 
{AVDELNINGTEMPT.I}


&Scoped-define SHARED 
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE bestapph AS HANDLE NO-UNDO.
DEFINE VARIABLE autovem AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE epostvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE brwsortvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE defrow AS ROWID EXTENT 4 NO-UNDO.
DEFINE VARIABLE fbestapph AS HANDLE NO-UNDO.
DEFINE VARIABLE fbok AS LOGICAL NO-UNDO.
DEFINE VARIABLE entryepost AS LOGICAL NO-UNDO.
DEFINE TEMP-TABLE fbtemp NO-UNDO
   FIELD NAMN AS CHARACTER
   FIELD ANV AS CHARACTER
   INDEX ANV ANV
   INDEX NAMN NAMN.

{EXECLIN2.I}
{KONTAKTPERSTEMP.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-3
&Scoped-define BROWSE-NAME BRW_BEF

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES kundbeftemp defpristemp kontaktperstemp ~
kundovertemp bestkundallt

/* Definitions for BROWSE BRW_BEF                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_BEF kundbeftemp.NAMN kundbeftemp.PRISA ~
kundbeftemp.PRISRES 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BEF kundbeftemp.NAMN 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_BEF kundbeftemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_BEF kundbeftemp
&Scoped-define QUERY-STRING-BRW_BEF FOR EACH kundbeftemp OF bestkundallt NO-LOCK
&Scoped-define OPEN-QUERY-BRW_BEF OPEN QUERY BRW_BEF FOR EACH kundbeftemp OF bestkundallt NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_BEF kundbeftemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BEF kundbeftemp


/* Definitions for BROWSE BRW_DPRISBEF                                  */
&Scoped-define FIELDS-IN-QUERY-BRW_DPRISBEF defpristemp.PRISNUM ~
defpristemp.PRISTEXT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_DPRISBEF defpristemp.PRISNUM 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_DPRISBEF defpristemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_DPRISBEF defpristemp
&Scoped-define QUERY-STRING-BRW_DPRISBEF FOR EACH defpristemp WHERE TRUE /* Join to bestkundallt incomplete */ NO-LOCK
&Scoped-define OPEN-QUERY-BRW_DPRISBEF OPEN QUERY BRW_DPRISBEF FOR EACH defpristemp WHERE TRUE /* Join to bestkundallt incomplete */ NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_DPRISBEF defpristemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_DPRISBEF defpristemp


/* Definitions for BROWSE BRW_DPRISOVER                                 */
&Scoped-define FIELDS-IN-QUERY-BRW_DPRISOVER defpristemp.PRISNUM ~
defpristemp.PRISTEXT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_DPRISOVER defpristemp.PRISNUM 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_DPRISOVER defpristemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_DPRISOVER defpristemp
&Scoped-define QUERY-STRING-BRW_DPRISOVER FOR EACH defpristemp WHERE TRUE /* Join to bestkundallt incomplete */ NO-LOCK
&Scoped-define OPEN-QUERY-BRW_DPRISOVER OPEN QUERY BRW_DPRISOVER FOR EACH defpristemp WHERE TRUE /* Join to bestkundallt incomplete */ NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_DPRISOVER defpristemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_DPRISOVER defpristemp


/* Definitions for BROWSE BRW_KONTAKT                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_KONTAKT kontaktperstemp.NAMN ~
kontaktperstemp.EPOST kontaktperstemp.TELEFON kontaktperstemp.MOBIL ~
kontaktperstemp.FAX 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KONTAKT kontaktperstemp.NAMN ~
kontaktperstemp.EPOST kontaktperstemp.TELEFON kontaktperstemp.MOBIL ~
kontaktperstemp.FAX 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_KONTAKT kontaktperstemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_KONTAKT kontaktperstemp
&Scoped-define QUERY-STRING-BRW_KONTAKT FOR EACH kontaktperstemp WHERE TRUE /* Join to bestkundallt incomplete */ NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KONTAKT OPEN QUERY BRW_KONTAKT FOR EACH kontaktperstemp WHERE TRUE /* Join to bestkundallt incomplete */ NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KONTAKT kontaktperstemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KONTAKT kontaktperstemp


/* Definitions for BROWSE BRW_KUNDO                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_KUNDO kundovertemp.OTEXT ~
kundovertemp.DAGTYP kundovertemp.START kundovertemp.SLUT kundovertemp.PRISA 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KUNDO kundovertemp.OTEXT 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_KUNDO kundovertemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_KUNDO kundovertemp
&Scoped-define QUERY-STRING-BRW_KUNDO FOR EACH kundovertemp OF bestkundallt NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KUNDO OPEN QUERY BRW_KUNDO FOR EACH kundovertemp OF bestkundallt NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KUNDO kundovertemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KUNDO kundovertemp


/* Definitions for DIALOG-BOX DIALOG-3                                  */
&Scoped-define QUERY-STRING-DIALOG-3 FOR EACH bestkundallt NO-LOCK
&Scoped-define OPEN-QUERY-DIALOG-3 OPEN QUERY DIALOG-3 FOR EACH bestkundallt NO-LOCK.
&Scoped-define TABLES-IN-QUERY-DIALOG-3 bestkundallt
&Scoped-define FIRST-TABLE-IN-QUERY-DIALOG-3 bestkundallt


/* Definitions for FRAME FRAME-GRUND                                    */

/* Definitions for FRAME FRAME-OVER                                     */

/* Definitions for FRAME FRAME-PRISBEF                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS bestkundallt.VIBESTID bestkundallt.BESTNAMN 
&Scoped-define ENABLED-TABLES bestkundallt
&Scoped-define FIRST-ENABLED-TABLE bestkundallt
&Scoped-Define ENABLED-OBJECTS RAD_VAL FILL-IN-KUNDNAMN2 FILL-IN-ORG BTN_OK ~
BTN_AVB 
&Scoped-Define DISPLAYED-FIELDS bestkundallt.VIBESTID bestkundallt.BESTNAMN 
&Scoped-define DISPLAYED-TABLES bestkundallt
&Scoped-define FIRST-DISPLAYED-TABLE bestkundallt
&Scoped-Define DISPLAYED-OBJECTS RAD_VAL FILL-IN-KUNDNAMN2 FILL-IN-ORG 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD tabortblanka DIALOG-3 
FUNCTION tabortblanka RETURNS CHARACTER
  ( INPUT namnblanka AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-KUNDNAMN2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Extra namn" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ORG AS CHARACTER FORMAT "999999-9999":U 
     LABEL "Org/Persnr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Grundregistreringar", 1
     SIZE 77 BY 1 NO-UNDO.

DEFINE BUTTON BTN_ANDRA 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_FRANEXCEL 
     LABEL "Importera Kontakter" 
     SIZE 14 BY 1.5 TOOLTIP "Importera Kontakter från Excel".

DEFINE BUTTON BTN_NY 
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_OUTLOOK 
     LABEL "Från Outlook" 
     SIZE 14 BY 1.5.

DEFINE VARIABLE CMB_BESTAVD AS CHARACTER FORMAT "X(256)":U 
     LABEL "Avdelning" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Ej kopplad","Allmän" 
     DROP-DOWN-LIST
     SIZE 18 BY 1
     FONT 4 NO-UNDO.

DEFINE VARIABLE FILL-IN-AVD AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tillhör avdelning" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1
     FONT 4 NO-UNDO.

DEFINE VARIABLE FILL-IN-EPOST AS CHARACTER FORMAT "X(256)":U 
     LABEL "E-post" 
     VIEW-AS FILL-IN 
     SIZE 49.63 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_FB AS CHARACTER FORMAT "X(20)":U 
     LABEL "Faktura beställare" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Ingen" 
     DROP-DOWN-LIST
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_KOST AS CHARACTER FORMAT "X(14)":U 
     LABEL "Kostnadsregistrering" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Automatiska","Manuellt","Inga" 
     DROP-DOWN-LIST
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_KUND AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kundtyp" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "CCC" 
     DROP-DOWN-LIST
     SIZE 17.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_LON AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lönetillägg" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Endast utvalda löne.","Inga" 
     DROP-DOWN-LIST
     SIZE 23.5 BY 1 TOOLTIP "Endast utvalda lönetillägg" NO-UNDO.

DEFINE VARIABLE CMB_MTRL AS CHARACTER FORMAT "X(14)":U 
     LABEL "Materiel" 
     VIEW-AS COMBO-BOX INNER-LINES 1
     LIST-ITEMS "Inga" 
     DROP-DOWN-LIST
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OVER AS CHARACTER FORMAT "X(14)":U 
     LABEL "Övertid" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Tidredovisning","Egna regler","Inga" 
     DROP-DOWN-LIST
     SIZE 23.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_OVR AS CHARACTER FORMAT "X(14)":U 
     LABEL "Övriga kostnader" 
     VIEW-AS COMBO-BOX INNER-LINES 1
     LIST-ITEMS "Inga" 
     DROP-DOWN-LIST
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_TIM AS CHARACTER FORMAT "X(14)":U 
     LABEL "Timpris" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Tidredovisning","Kundens egna","Inga" 
     DROP-DOWN-LIST
     SIZE 23.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_TRA AS CHARACTER FORMAT "X(14)":U 
     LABEL "Traktamente" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Tidredovisning","Eget pris","Inga" 
     DROP-DOWN-LIST
     SIZE 23.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-MOTPART AS CHARACTER FORMAT "X(256)":U 
     LABEL "Motpart" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_ENTRAK AS DECIMAL FORMAT "->>>>9.99" INITIAL 0 
     LABEL "Endagsförrättning i kr" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE FILL-IN_FAKINT AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Antal dagar mellan fakturering" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_FDAGAR AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Förfallodagar" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_FLERTRAK AS DECIMAL FORMAT "->>>>9.99" INITIAL 0 
     LABEL "Flerdygnsförrättning i kr" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE FILL-IN_FRTJPA AS DECIMAL FORMAT "->>9" INITIAL 0 
     LABEL "Fr. tjänst i %" 
     VIEW-AS FILL-IN 
     SIZE 6.75 BY 1.

DEFINE VARIABLE FILL-IN_MIL AS DECIMAL FORMAT "->>>>9.99" INITIAL 0 
     LABEL "Milersättning kr/mil" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE FILL-IN_MTRLPA AS DECIMAL FORMAT "->>9" INITIAL 0 
     LABEL "Materiel i %" 
     VIEW-AS FILL-IN 
     SIZE 6.75 BY 1.

DEFINE VARIABLE FILL-IN_SLUT% AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "och Slut%" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_START% AS INTEGER FORMAT ">>9" INITIAL 0 
     LABEL "Start%" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_BEF FOR 
      kundbeftemp SCROLLING.

DEFINE QUERY BRW_DPRISBEF FOR 
      defpristemp SCROLLING.

DEFINE QUERY BRW_DPRISOVER FOR 
      defpristemp SCROLLING.

DEFINE QUERY BRW_KONTAKT FOR 
      kontaktperstemp SCROLLING.

DEFINE QUERY BRW_KUNDO FOR 
      kundovertemp SCROLLING.

DEFINE QUERY DIALOG-3 FOR 
      bestkundallt SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_BEF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BEF DIALOG-3 _STRUCTURED
  QUERY BRW_BEF NO-LOCK DISPLAY
      kundbeftemp.NAMN FORMAT "x(20)":U
      kundbeftemp.PRISA COLUMN-LABEL "Tim pris" FORMAT "->>>>9.99":U
      kundbeftemp.PRISRES COLUMN-LABEL "Res pris" FORMAT "->>>>9.99":U
  ENABLE
      kundbeftemp.NAMN
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 42.38 BY 15
         TITLE "Priser".

DEFINE BROWSE BRW_DPRISBEF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_DPRISBEF DIALOG-3 _STRUCTURED
  QUERY BRW_DPRISBEF NO-LOCK DISPLAY
      defpristemp.PRISNUM COLUMN-LABEL "Prislista" FORMAT "->>>>>>9":U
      defpristemp.PRISTEXT COLUMN-LABEL "Text" FORMAT "X(25)":U
  ENABLE
      defpristemp.PRISNUM
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 37.75 BY 15
         TITLE "Valbara prislistor".

DEFINE BROWSE BRW_DPRISOVER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_DPRISOVER DIALOG-3 _STRUCTURED
  QUERY BRW_DPRISOVER NO-LOCK DISPLAY
      defpristemp.PRISNUM COLUMN-LABEL "Pris!lista" FORMAT "->>>>>>9":U
            WIDTH 4.5
      defpristemp.PRISTEXT COLUMN-LABEL "Benämning" FORMAT "X(25)":U
  ENABLE
      defpristemp.PRISNUM
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 37 BY 15
         TITLE "Övertidsprislistor".

DEFINE BROWSE BRW_KONTAKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KONTAKT DIALOG-3 _STRUCTURED
  QUERY BRW_KONTAKT NO-LOCK DISPLAY
      kontaktperstemp.NAMN COLUMN-LABEL "Namn" FORMAT "X(256)":U
            WIDTH 17
      kontaktperstemp.EPOST COLUMN-LABEL "E-post" FORMAT "X(256)":U
            WIDTH 20.25
      kontaktperstemp.TELEFON COLUMN-LABEL "Telefon" FORMAT "X(256)":U
            WIDTH 7.5
      kontaktperstemp.MOBIL COLUMN-LABEL "Mobil" FORMAT "X(256)":U
            WIDTH 7.5
      kontaktperstemp.FAX COLUMN-LABEL "Fax" FORMAT "X(256)":U
            WIDTH 7.5
  ENABLE
      kontaktperstemp.NAMN
      kontaktperstemp.EPOST
      kontaktperstemp.TELEFON
      kontaktperstemp.MOBIL
      kontaktperstemp.FAX
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 61 BY 13.75
         FONT 4
         TITLE "Alla kontaktpersoner" ROW-HEIGHT-CHARS .54 FIT-LAST-COLUMN.

DEFINE BROWSE BRW_KUNDO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KUNDO DIALOG-3 _STRUCTURED
  QUERY BRW_KUNDO NO-LOCK DISPLAY
      kundovertemp.OTEXT COLUMN-LABEL "Övertid" FORMAT "X(25)":U
      kundovertemp.DAGTYP COLUMN-LABEL "Dagtyp" FORMAT "X(8)":U
      kundovertemp.START COLUMN-LABEL "Start!tid" FORMAT "99.99":U
            COLUMN-FGCOLOR 1 LABEL-FGCOLOR 1
      kundovertemp.SLUT COLUMN-LABEL "Slut!tid" FORMAT "99.99":U
            COLUMN-FGCOLOR 3 LABEL-FGCOLOR 3
      kundovertemp.PRISA COLUMN-LABEL "Pris" FORMAT "->>>>9.99":U
  ENABLE
      kundovertemp.OTEXT
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 57.38 BY 15
         TITLE "Övertidsprislista".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-3
     RAD_VAL AT ROW 1.38 COL 1.75 NO-LABEL
     bestkundallt.VIBESTID AT ROW 2.46 COL 10 COLON-ALIGNED
          LABEL "Best-id" FORMAT "X(10)"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     bestkundallt.BESTNAMN AT ROW 2.46 COL 28.88
          LABEL "Namn" FORMAT "x(25)"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     FILL-IN-KUNDNAMN2 AT ROW 2.46 COL 66.75 WIDGET-ID 4
     FILL-IN-ORG AT ROW 2.46 COL 108.5 COLON-ALIGNED WIDGET-ID 2
     BTN_OK AT ROW 26 COL 96.25
     BTN_AVB AT ROW 26 COL 111.25
     SPACE(0.74) SKIP(0.12)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Beställare/Kund":L.

DEFINE FRAME FRAME-REGLER
     FILL-IN-MOTPART AT ROW 2.88 COL 78.38 COLON-ALIGNED
     CMB_KUND AT ROW 2.92 COL 29.13 COLON-ALIGNED
     FILL-IN_FDAGAR AT ROW 4.08 COL 29.13 COLON-ALIGNED
     FILL-IN_FAKINT AT ROW 4.08 COL 78.38 COLON-ALIGNED
     CMB_TIM AT ROW 6.67 COL 29.13 COLON-ALIGNED
     CMB_KOST AT ROW 6.67 COL 78.38 COLON-ALIGNED
     CMB_OVER AT ROW 7.83 COL 29.13 COLON-ALIGNED
     CMB_MTRL AT ROW 7.83 COL 78.38 COLON-ALIGNED
     CMB_LON AT ROW 9 COL 29.13 COLON-ALIGNED
     CMB_OVR AT ROW 9 COL 78.38 COLON-ALIGNED
     CMB_TRA AT ROW 10.17 COL 29.13 COLON-ALIGNED
     CMB_FB AT ROW 10.17 COL 78.38 COLON-ALIGNED
     FILL-IN_ENTRAK AT ROW 12.63 COL 29.13 COLON-ALIGNED
     FILL-IN_FLERTRAK AT ROW 13.83 COL 29.13 COLON-ALIGNED
     FILL-IN_MIL AT ROW 13.83 COL 78.38 COLON-ALIGNED
     FILL-IN_START% AT ROW 16.42 COL 29.13 COLON-ALIGNED
     FILL-IN_SLUT% AT ROW 16.42 COL 78.38 COLON-ALIGNED
     FILL-IN_FRTJPA AT ROW 18.71 COL 29.13 COLON-ALIGNED
     FILL-IN_MTRLPA AT ROW 18.71 COL 78.38 COLON-ALIGNED
     "Procent vid arbetets:" VIEW-AS TEXT
          SIZE 28.38 BY 1.17 TOOLTIP "Endast vid fastpris och A-contofakturering" AT ROW 15.04 COL 1.5
          FONT 17
     "Ange varifrån de olika uppgifterna skall hämtas:" VIEW-AS TEXT
          SIZE 55.13 BY 1.17 AT ROW 5.29 COL 1.5
          FONT 17
     "Uppläggning av regler för fakturering:" VIEW-AS TEXT
          SIZE 43.63 BY 1.13 AT ROW 1.58 COL 1.5
          FONT 17
     "Procentpåslag för:" VIEW-AS TEXT
          SIZE 25.88 BY 1 AT ROW 17.63 COL 1.5
          FONT 17
     "Ersättning:" VIEW-AS TEXT
          SIZE 16.88 BY 1.08 AT ROW 11.33 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 4
         SIZE 125 BY 21.

DEFINE FRAME FRAME-PRISBEF
     BRW_DPRISBEF AT ROW 5 COL 1.5
     BRW_BEF AT ROW 5 COL 41.75
     "Välj prislista." VIEW-AS TEXT
          SIZE 41 BY 2 AT ROW 1.67 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 4
         SIZE 125 BY 21.

DEFINE FRAME FRAME-OVER
     BRW_DPRISOVER AT ROW 5 COL 1.5
     BRW_KUNDO AT ROW 5 COL 40.13
     "Då det normala övertidsavtalet ej ska gälla." VIEW-AS TEXT
          SIZE 51.75 BY 1.46 AT ROW 3.25 COL 1.5
          FONT 17
     "Uppläggning av övertidsregler för fakturering till kund:" VIEW-AS TEXT
          SIZE 67.13 BY 1.5 AT ROW 1.75 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 4
         SIZE 125 BY 21.

DEFINE FRAME FRAME-GRUND
     BTN_FRANEXCEL AT ROW 1 COL 111 WIDGET-ID 2
     BTN_OUTLOOK AT ROW 1.13 COL 111
     FILL-IN-AVD AT ROW 1.58 COL 9.63 COLON-ALIGNED
     CMB_BESTAVD AT ROW 1.58 COL 45.13 COLON-ALIGNED
     bestkundallt.KONTAKT AT ROW 2.75 COL 9.63 COLON-ALIGNED
          LABEL "Kontaktperson" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 49.63 BY 1
     BRW_KONTAKT AT ROW 2.75 COL 64
     bestkundallt.TEL AT ROW 3.92 COL 9.63 COLON-ALIGNED
          LABEL "Tel" FORMAT "X(256)"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
          FONT 4
     bestkundallt.FAXNR AT ROW 3.92 COL 39.25 COLON-ALIGNED
          LABEL "Mobilnr" FORMAT "X(256)"
          VIEW-AS FILL-IN 
          SIZE 20 BY 1
     FILL-IN-EPOST AT ROW 5.08 COL 9.63 COLON-ALIGNED
     bestkundallt.ADRESS AT ROW 6.25 COL 9.63 COLON-ALIGNED
          LABEL "Adress" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 49.63 BY 1
          FONT 4
     bestkundallt.PNR AT ROW 7.33 COL 9.63 COLON-ALIGNED
          LABEL "Postnr"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
          FONT 4
     bestkundallt.ORT AT ROW 7.33 COL 32.25 COLON-ALIGNED
          LABEL "Ort" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          FONT 4
     bestkundallt.LEVADRESS AT ROW 9.42 COL 9.63 COLON-ALIGNED
          LABEL "Leveransadress" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 49.63 BY 1
          FONT 4
     bestkundallt.LEVPNR AT ROW 10.67 COL 9.63 COLON-ALIGNED
          LABEL "Postnr"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
          FONT 4
     bestkundallt.LEVORT AT ROW 10.67 COL 32.25 COLON-ALIGNED
          LABEL "Ort" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          FONT 4
     bestkundallt.FAKADRESS AT ROW 12.54 COL 9.63 COLON-ALIGNED
          LABEL "Fakturaadress" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 49.63 BY 1
          FONT 4
     bestkundallt.FAKPNR AT ROW 13.79 COL 9.63 COLON-ALIGNED
          LABEL "Postnr"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
          FONT 4
     bestkundallt.FAKORT AT ROW 13.79 COL 32.25 COLON-ALIGNED
          LABEL "Ort" FORMAT "x(256)"
          VIEW-AS FILL-IN 
          SIZE 27 BY 1
          FONT 4
     bestkundallt.AOKUND AT ROW 15.67 COL 32.25 COLON-ALIGNED
          LABEL "Koppling till aonr"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
          FONT 4
     BTN_NY AT ROW 17.25 COL 79.88
     BTN_ANDRA AT ROW 17.25 COL 93
     BTN_BORT AT ROW 17.25 COL 106.13
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 4
         SIZE 125 BY 21
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: bestkundallt T "?" NO-UNDO temp-db bestkundallt
      TABLE: defpristemp T "?" NO-UNDO temp-db defpristemp
      TABLE: kontaktperstemp T "?" NO-UNDO temp-db kontaktperstemp
      TABLE: kundbeftemp T "?" NO-UNDO temp-db kundbeftemp
      TABLE: kundovertemp T "?" NO-UNDO temp-db kundovertemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* REPARENT FRAME */
ASSIGN FRAME FRAME-GRUND:FRAME = FRAME DIALOG-3:HANDLE
       FRAME FRAME-OVER:FRAME = FRAME DIALOG-3:HANDLE
       FRAME FRAME-PRISBEF:FRAME = FRAME DIALOG-3:HANDLE
       FRAME FRAME-REGLER:FRAME = FRAME DIALOG-3:HANDLE.

/* SETTINGS FOR DIALOG-BOX DIALOG-3
   NOT-VISIBLE FRAME-NAME                                               */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME FRAME-GRUND:MOVE-AFTER-TAB-ITEM (FILL-IN-ORG:HANDLE IN FRAME DIALOG-3)
       XXTABVALXX = FRAME FRAME-REGLER:MOVE-BEFORE-TAB-ITEM (BTN_OK:HANDLE IN FRAME DIALOG-3)
       XXTABVALXX = FRAME FRAME-PRISBEF:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-REGLER:HANDLE)
       XXTABVALXX = FRAME FRAME-OVER:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-PRISBEF:HANDLE)
       XXTABVALXX = FRAME FRAME-GRUND:MOVE-BEFORE-TAB-ITEM (FRAME FRAME-OVER:HANDLE)
/* END-ASSIGN-TABS */.

ASSIGN 
       FRAME DIALOG-3:SCROLLABLE       = FALSE
       FRAME DIALOG-3:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN bestkundallt.BESTNAMN IN FRAME DIALOG-3
   ALIGN-L EXP-LABEL EXP-FORMAT                                         */
/* SETTINGS FOR FILL-IN FILL-IN-KUNDNAMN2 IN FRAME DIALOG-3
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN bestkundallt.VIBESTID IN FRAME DIALOG-3
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FRAME FRAME-GRUND
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_KONTAKT KONTAKT FRAME-GRUND */
ASSIGN 
       FRAME FRAME-GRUND:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN bestkundallt.ADRESS IN FRAME FRAME-GRUND
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN bestkundallt.AOKUND IN FRAME FRAME-GRUND
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN bestkundallt.FAKADRESS IN FRAME FRAME-GRUND
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN bestkundallt.FAKORT IN FRAME FRAME-GRUND
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN bestkundallt.FAKPNR IN FRAME FRAME-GRUND
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN bestkundallt.FAXNR IN FRAME FRAME-GRUND
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       bestkundallt.FAXNR:READ-ONLY IN FRAME FRAME-GRUND        = TRUE.

ASSIGN 
       FILL-IN-EPOST:READ-ONLY IN FRAME FRAME-GRUND        = TRUE.

/* SETTINGS FOR FILL-IN bestkundallt.KONTAKT IN FRAME FRAME-GRUND
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       bestkundallt.KONTAKT:READ-ONLY IN FRAME FRAME-GRUND        = TRUE.

/* SETTINGS FOR FILL-IN bestkundallt.LEVADRESS IN FRAME FRAME-GRUND
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN bestkundallt.LEVORT IN FRAME FRAME-GRUND
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN bestkundallt.LEVPNR IN FRAME FRAME-GRUND
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN bestkundallt.ORT IN FRAME FRAME-GRUND
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN bestkundallt.PNR IN FRAME FRAME-GRUND
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN bestkundallt.TEL IN FRAME FRAME-GRUND
   EXP-LABEL EXP-FORMAT                                                 */
ASSIGN 
       bestkundallt.TEL:READ-ONLY IN FRAME FRAME-GRUND        = TRUE.

/* SETTINGS FOR FRAME FRAME-OVER
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_DPRISOVER TEXT-2 FRAME-OVER */
/* BROWSE-TAB BRW_KUNDO BRW_DPRISOVER FRAME-OVER */
ASSIGN 
       FRAME FRAME-OVER:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-PRISBEF
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_DPRISBEF TEXT-3 FRAME-PRISBEF */
/* BROWSE-TAB BRW_BEF BRW_DPRISBEF FRAME-PRISBEF */
ASSIGN 
       FRAME FRAME-PRISBEF:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME FRAME-REGLER
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FRAME-REGLER:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_FB IN FRAME FRAME-REGLER
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       CMB_FB:HIDDEN IN FRAME FRAME-REGLER           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_MTRL IN FRAME FRAME-REGLER
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       CMB_MTRL:HIDDEN IN FRAME FRAME-REGLER           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_OVR IN FRAME FRAME-REGLER
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       CMB_OVR:HIDDEN IN FRAME FRAME-REGLER           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-MOTPART IN FRAME FRAME-REGLER
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-MOTPART:HIDDEN IN FRAME FRAME-REGLER           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BEF
/* Query rebuild information for BROWSE BRW_BEF
     _TblList          = "Temp-Tables.kundbeftemp OF Temp-Tables.bestkundallt"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kundbeftemp.NAMN
"kundbeftemp.NAMN" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.kundbeftemp.PRISA
"kundbeftemp.PRISA" "Tim pris" "->>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.kundbeftemp.PRISRES
"kundbeftemp.PRISRES" "Res pris" "->>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_BEF */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_DPRISBEF
/* Query rebuild information for BROWSE BRW_DPRISBEF
     _TblList          = "Temp-Tables.defpristemp WHERE Temp-Tables.bestkundallt <external> ..."
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.defpristemp.PRISNUM
"defpristemp.PRISNUM" "Prislista" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.defpristemp.PRISTEXT
"defpristemp.PRISTEXT" "Text" "X(25)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_DPRISBEF */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_DPRISOVER
/* Query rebuild information for BROWSE BRW_DPRISOVER
     _TblList          = "Temp-Tables.defpristemp WHERE Temp-Tables.bestkundallt <external> ..."
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.defpristemp.PRISNUM
"defpristemp.PRISNUM" "Pris!lista" ? "integer" ? ? ? ? ? ? yes ? no no "4.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.defpristemp.PRISTEXT
"defpristemp.PRISTEXT" "Benämning" "X(25)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_DPRISOVER */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KONTAKT
/* Query rebuild information for BROWSE BRW_KONTAKT
     _TblList          = "Temp-Tables.kontaktperstemp WHERE Temp-Tables.bestkundallt <external> ..."
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kontaktperstemp.NAMN
"kontaktperstemp.NAMN" "Namn" "X(256)" "character" ? ? ? ? ? ? yes ? no no "17" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.kontaktperstemp.EPOST
"kontaktperstemp.EPOST" "E-post" "X(256)" "character" ? ? ? ? ? ? yes ? no no "20.25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.kontaktperstemp.TELEFON
"kontaktperstemp.TELEFON" "Telefon" "X(256)" "character" ? ? ? ? ? ? yes ? no no "7.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.kontaktperstemp.MOBIL
"kontaktperstemp.MOBIL" "Mobil" "X(256)" "character" ? ? ? ? ? ? yes ? no no "7.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.kontaktperstemp.FAX
"kontaktperstemp.FAX" "Fax" "X(256)" "character" ? ? ? ? ? ? yes ? no no "7.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KONTAKT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KUNDO
/* Query rebuild information for BROWSE BRW_KUNDO
     _TblList          = "Temp-Tables.kundovertemp OF Temp-Tables.bestkundallt"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kundovertemp.OTEXT
"kundovertemp.OTEXT" "Övertid" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.kundovertemp.DAGTYP
"kundovertemp.DAGTYP" "Dagtyp" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.kundovertemp.START
"kundovertemp.START" "Start!tid" ? "decimal" ? 1 ? ? 1 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.kundovertemp.SLUT
"kundovertemp.SLUT" "Slut!tid" ? "decimal" ? 3 ? ? 3 ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.kundovertemp.PRISA
"kundovertemp.PRISA" "Pris" "->>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KUNDO */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-3
/* Query rebuild information for DIALOG-BOX DIALOG-3
     _TblList          = "Temp-Tables.bestkundallt"
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-GRUND
/* Query rebuild information for FRAME FRAME-GRUND
     _Query            is NOT OPENED
*/  /* FRAME FRAME-GRUND */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-OVER
/* Query rebuild information for FRAME FRAME-OVER
     _Query            is NOT OPENED
*/  /* FRAME FRAME-OVER */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-PRISBEF
/* Query rebuild information for FRAME FRAME-PRISBEF
     _Query            is NOT OPENED
*/  /* FRAME FRAME-PRISBEF */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-REGLER
/* Query rebuild information for FRAME FRAME-REGLER
     _Query            is NOT OPENED
*/  /* FRAME FRAME-REGLER */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-3 DIALOG-3
ON END-ERROR OF FRAME DIALOG-3 /* Beställare/Kund */
DO:
   {muswait.i}
   IF VALID-HANDLE(fbestapph) THEN DELETE PROCEDURE fbestapph.      
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   musz = TRUE.
   {BORTBRWPROC.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-3 DIALOG-3
ON ENDKEY OF FRAME DIALOG-3 /* Beställare/Kund */
DO:
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-GRUND
&Scoped-define SELF-NAME bestkundallt.AOKUND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bestkundallt.AOKUND DIALOG-3
ON MOUSE-SELECT-CLICK OF bestkundallt.AOKUND IN FRAME FRAME-GRUND /* Koppling till aonr */
DO:                 
   IF INPUT bestkundallt.AOKUND = TRUE THEN bestkundallt.AOKUND = FALSE. 
   IF INPUT bestkundallt.AOKUND = FALSE THEN bestkundallt.AOKUND = TRUE. 
   DISPLAY bestkundallt.AOKUND WITH FRAME {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DIALOG-3
&Scoped-define SELF-NAME bestkundallt.BESTNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bestkundallt.BESTNAMN DIALOG-3
ON ENTRY OF bestkundallt.BESTNAMN IN FRAME DIALOG-3 /* Namn */
DO:
   RUN tabstop_UI (INPUT FALSE).   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_BEF
&Scoped-define FRAME-NAME FRAME-PRISBEF
&Scoped-define SELF-NAME BRW_BEF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_BEF DIALOG-3
ON ENTRY OF BRW_BEF IN FRAME FRAME-PRISBEF /* Priser */
DO:
   RUN tabstop_UI (INPUT TRUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_DPRISBEF
&Scoped-define SELF-NAME BRW_DPRISBEF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_DPRISBEF DIALOG-3
ON VALUE-CHANGED OF BRW_DPRISBEF IN FRAME FRAME-PRISBEF /* Valbara prislistor */
DO:         
   defrow[2] = ROWID(defpristemp).
   /*
   IF fragakoll = TRUE THEN DO:
      fragakoll = FALSE.
   END.
   ELSE DO:
   */
   status-ok = BRW_DPRISBEF:SELECT-FOCUSED-ROW() IN FRAME FRAME-PRISBEF.
   RUN nylista_UI (INPUT 1).      
   IF musz = TRUE THEN musz = FALSE.
   ELSE DO:
      RUN setlastrowid_UI IN brwproc[2] (INPUT defrow[2]).              
      RUN lastselectdyn_UI IN brwproc[2].        
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_DPRISOVER
&Scoped-define FRAME-NAME FRAME-OVER
&Scoped-define SELF-NAME BRW_DPRISOVER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_DPRISOVER DIALOG-3
ON VALUE-CHANGED OF BRW_DPRISOVER IN FRAME FRAME-OVER /* Övertidsprislistor */
DO:         
   /*
   IF fragakoll = TRUE THEN DO:
      fragakoll = FALSE.
   END.
   ELSE DO:
   */
   defrow[3] = ROWID(defpristemp).
   status-ok = BRW_DPRISOVER:SELECT-FOCUSED-ROW() IN FRAME FRAME-OVER.
   RUN nylista_UI (INPUT 2).      
   IF musz = TRUE THEN musz = FALSE.
   ELSE DO:
      RUN setlastrowid_UI IN brwproc[3] (INPUT defrow[3]).              
      RUN lastselectdyn_UI IN brwproc[3].        
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KONTAKT
&Scoped-define FRAME-NAME FRAME-GRUND
&Scoped-define SELF-NAME BRW_KONTAKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KONTAKT DIALOG-3
ON MOUSE-SELECT-DBLCLICK OF BRW_KONTAKT IN FRAME FRAME-GRUND /* Alla kontaktpersoner */
DO:
   IF AVAILABLE kontaktperstemp THEN DO:
      ASSIGN
      FILL-IN-EPOST          = kontaktperstemp.EPOST   
      bestkundallt.FAXNR     =  kontaktperstemp.MOBIL   
      bestkundallt.KONTAKT   =  kontaktperstemp.NAMN    
      bestkundallt.TEL        = kontaktperstemp.TELEFON. 
      DISPLAY FILL-IN-EPOST bestkundallt.FAXNR  bestkundallt.KONTAKT bestkundallt.TEL WITH FRAME FRAME-GRUND.   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KONTAKT DIALOG-3
ON ROW-LEAVE OF BRW_KONTAKT IN FRAME FRAME-GRUND /* Alla kontaktpersoner */
DO:
   status-ok = BRW_KONTAKT:SELECT-FOCUSED-ROW() IN FRAME FRAME-GRUND NO-ERROR.
   IF AVAILABLE kontaktperstemp THEN DO:
      DISPLAY  
      kontaktperstemp.NAMN      
      kontaktperstemp.EPOST   
      kontaktperstemp.TELEFON 
      kontaktperstemp.MOBIL   
      kontaktperstemp.FAX
      WITH BROWSE BRW_KONTAKT.
      kontaktperstemp.NAMN    = INPUT BROWSE BRW_KONTAKT kontaktperstemp.NAMN   .
      kontaktperstemp.EPOST   = INPUT BROWSE BRW_KONTAKT kontaktperstemp.EPOST  .
      kontaktperstemp.TELEFON = INPUT BROWSE BRW_KONTAKT kontaktperstemp.TELEFON.
      kontaktperstemp.MOBIL   = INPUT BROWSE BRW_KONTAKT kontaktperstemp.MOBIL  .
      kontaktperstemp.FAX   = INPUT BROWSE BRW_KONTAKT kontaktperstemp.FAX  .
      DISPLAY  kontaktperstemp.EPOST WITH BROWSE BRW_KONTAKT.
   END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KONTAKT DIALOG-3
ON VALUE-CHANGED OF BRW_KONTAKT IN FRAME FRAME-GRUND /* Alla kontaktpersoner */
DO:
  status-ok = BRW_KONTAKT:SELECT-FOCUSED-ROW() IN FRAME FRAME-GRUND NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kontaktperstemp.NAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kontaktperstemp.NAMN BRW_KONTAKT _BROWSE-COLUMN DIALOG-3
ON ENTRY OF kontaktperstemp.NAMN IN BROWSE BRW_KONTAKT /* Namn */
DO:
   entryepost = TRUE.
   IF AVAILABLE kontaktperstemp THEN DISPLAY kontaktperstemp.NAMN kontaktperstemp.EPOST kontaktperstemp.TELEFON
   kontaktperstemp.MOBIL kontaktperstemp.FAX WITH BROWSE BRW_KONTAKT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kontaktperstemp.NAMN BRW_KONTAKT _BROWSE-COLUMN DIALOG-3
ON LEAVE OF kontaktperstemp.NAMN IN BROWSE BRW_KONTAKT /* Namn */
DO:
   IF AVAILABLE kontaktperstemp THEN DO:
      IF kontaktperstemp.NAMN NE INPUT BROWSE BRW_KONTAKT kontaktperstemp.NAMN THEN DO:
         kontaktperstemp.NAMN = INPUT BROWSE BRW_KONTAKT kontaktperstemp.NAMN.   
         DISPLAY  kontaktperstemp.NAMN WITH BROWSE BRW_KONTAKT.         
      END.
      ELSE /*kontaktperstemp.NAMN:READ-ONLY IN BROWSE BRW_KONTAKT = TRUE.*/
   END.   
   entryepost = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kontaktperstemp.EPOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kontaktperstemp.EPOST BRW_KONTAKT _BROWSE-COLUMN DIALOG-3
ON ENTRY OF kontaktperstemp.EPOST IN BROWSE BRW_KONTAKT /* E-post */
DO:
   entryepost = TRUE.
   IF AVAILABLE kontaktperstemp THEN DISPLAY kontaktperstemp.NAMN kontaktperstemp.EPOST kontaktperstemp.TELEFON
   kontaktperstemp.MOBIL kontaktperstemp.FAX WITH BROWSE BRW_KONTAKT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kontaktperstemp.EPOST BRW_KONTAKT _BROWSE-COLUMN DIALOG-3
ON LEAVE OF kontaktperstemp.EPOST IN BROWSE BRW_KONTAKT /* E-post */
DO:
   IF AVAILABLE kontaktperstemp THEN DO:
      IF kontaktperstemp.EPOST NE INPUT BROWSE BRW_KONTAKT kontaktperstemp.EPOST THEN DO:
         kontaktperstemp.EPOST = INPUT BROWSE BRW_KONTAKT kontaktperstemp.EPOST.   
         DISPLAY  kontaktperstemp.EPOST WITH BROWSE BRW_KONTAKT.
         IF kontaktperstemp.EPOST NE "" THEN DO:
            RUN EPOSTKOLL.P (INPUT kontaktperstemp.EPOST,OUTPUT musz).
            IF musz = FALSE THEN DO:
               APPLY "ENTRY" TO kontaktperstemp.EPOST IN BROWSE BRW_KONTAKT.
               RETURN NO-APPLY.               
            END.               
         END.
         musz = FALSE.
      END.
      ELSE /*kontaktperstemp.EPOST:READ-ONLY IN BROWSE BRW_KONTAKT = TRUE.*/
   END.   
   entryepost = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kontaktperstemp.TELEFON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kontaktperstemp.TELEFON BRW_KONTAKT _BROWSE-COLUMN DIALOG-3
ON ENTRY OF kontaktperstemp.TELEFON IN BROWSE BRW_KONTAKT /* Telefon */
DO:
   entryepost = TRUE.
   IF AVAILABLE kontaktperstemp THEN DISPLAY kontaktperstemp.NAMN kontaktperstemp.EPOST kontaktperstemp.TELEFON
   kontaktperstemp.MOBIL kontaktperstemp.FAX WITH BROWSE BRW_KONTAKT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kontaktperstemp.TELEFON BRW_KONTAKT _BROWSE-COLUMN DIALOG-3
ON LEAVE OF kontaktperstemp.TELEFON IN BROWSE BRW_KONTAKT /* Telefon */
DO:
   IF AVAILABLE kontaktperstemp THEN DO:
      IF kontaktperstemp.TELEFON NE INPUT BROWSE BRW_KONTAKT kontaktperstemp.TELEFON THEN DO:
         kontaktperstemp.TELEFON = INPUT BROWSE BRW_KONTAKT kontaktperstemp.TELEFON. 
         kontaktperstemp.TELEFON = tabortblanka(kontaktperstemp.TELEFON ).    
         DISPLAY  kontaktperstemp.TELEFON WITH BROWSE BRW_KONTAKT.         
      END.
      ELSE /*kontaktperstemp.TELEFON:READ-ONLY IN BROWSE BRW_KONTAKT = TRUE.*/
   END.   
   entryepost = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kontaktperstemp.MOBIL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kontaktperstemp.MOBIL BRW_KONTAKT _BROWSE-COLUMN DIALOG-3
ON ENTRY OF kontaktperstemp.MOBIL IN BROWSE BRW_KONTAKT /* Mobil */
DO:
   entryepost = TRUE.
   IF AVAILABLE kontaktperstemp THEN DISPLAY kontaktperstemp.NAMN kontaktperstemp.EPOST kontaktperstemp.TELEFON
   kontaktperstemp.MOBIL kontaktperstemp.FAX WITH BROWSE BRW_KONTAKT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kontaktperstemp.MOBIL BRW_KONTAKT _BROWSE-COLUMN DIALOG-3
ON LEAVE OF kontaktperstemp.MOBIL IN BROWSE BRW_KONTAKT /* Mobil */
DO:
   IF AVAILABLE kontaktperstemp THEN DO:
      IF kontaktperstemp.MOBIL NE INPUT BROWSE BRW_KONTAKT kontaktperstemp.MOBIL THEN DO:
         kontaktperstemp.MOBIL = INPUT BROWSE BRW_KONTAKT kontaktperstemp.MOBIL. 
         kontaktperstemp.MOBIL = tabortblanka(kontaktperstemp.MOBIL ).    
         DISPLAY  kontaktperstemp.MOBIL WITH BROWSE BRW_KONTAKT.         
      END.
      ELSE /*kontaktperstemp.MOBIL:READ-ONLY IN BROWSE BRW_KONTAKT = TRUE.*/
   END.   
   entryepost = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME kontaktperstemp.FAX
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kontaktperstemp.FAX BRW_KONTAKT _BROWSE-COLUMN DIALOG-3
ON ENTRY OF kontaktperstemp.FAX IN BROWSE BRW_KONTAKT /* Fax */
DO:
   entryepost = TRUE.
   IF AVAILABLE kontaktperstemp THEN DISPLAY kontaktperstemp.NAMN kontaktperstemp.EPOST kontaktperstemp.TELEFON
   kontaktperstemp.MOBIL kontaktperstemp.FAX WITH BROWSE BRW_KONTAKT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL kontaktperstemp.FAX BRW_KONTAKT _BROWSE-COLUMN DIALOG-3
ON LEAVE OF kontaktperstemp.FAX IN BROWSE BRW_KONTAKT /* Fax */
DO:
   IF AVAILABLE kontaktperstemp THEN DO:
      IF kontaktperstemp.FAX NE INPUT BROWSE BRW_KONTAKT kontaktperstemp.FAX THEN DO:
         kontaktperstemp.FAX = INPUT BROWSE BRW_KONTAKT kontaktperstemp.FAX.   
         kontaktperstemp.FAX = tabortblanka(kontaktperstemp.FAX ).  
         DISPLAY  kontaktperstemp.FAX WITH BROWSE BRW_KONTAKT.         
      END.
      ELSE /*kontaktperstemp.NAMN:READ-ONLY IN BROWSE BRW_KONTAKT = TRUE.*/
   END.   
   entryepost = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KUNDO
&Scoped-define FRAME-NAME FRAME-OVER
&Scoped-define SELF-NAME BRW_KUNDO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KUNDO DIALOG-3
ON ENTRY OF BRW_KUNDO IN FRAME FRAME-OVER /* Övertidsprislista */
DO:
   RUN tabstop_UI (INPUT TRUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-GRUND
&Scoped-define SELF-NAME BTN_ANDRA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ANDRA DIALOG-3
ON CHOOSE OF BTN_ANDRA IN FRAME FRAME-GRUND /* Ändra */
DO:
   IF AVAILABLE kontaktperstemp THEN DO:
      ASSIGN
      kontaktperstemp.NAMN:READ-ONLY IN BROWSE BRW_KONTAKT = FALSE
      kontaktperstemp.EPOST:READ-ONLY IN BROWSE BRW_KONTAKT = FALSE
      kontaktperstemp.TELEFON:READ-ONLY IN BROWSE BRW_KONTAKT = FALSE
      kontaktperstemp.MOBIL:READ-ONLY IN BROWSE BRW_KONTAKT = FALSE.
      APPLY "ENTRY" TO kontaktperstemp.NAMN IN BROWSE BRW_KONTAKT. 
      DISPLAY BRW_KONTAKT WITH FRAME FRAME-GRUND.  
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DIALOG-3
&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-3
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-3 /* Avbryt */
DO:
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-GRUND
&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT DIALOG-3
ON CHOOSE OF BTN_BORT IN FRAME FRAME-GRUND /* Ta bort */
DO:
   status-ok = BRW_KONTAKT:SELECT-FOCUSED-ROW() IN FRAME FRAME-GRUND NO-ERROR.
   IF AVAILABLE kontaktperstemp THEN DO:
      MESSAGE "Vill du ta bort " kontaktperstemp.NAMN "?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Meddelande" UPDATE svar AS LOGICAL.
      IF svar = FALSE THEN RETURN NO-APPLY.
      DELETE kontaktperstemp.
      RUN selnextprevrow_UI IN brwproc[5].
      RUN openbdyn_UI IN brwproc[5] (INPUT "").
      RUN lastselectdyn_UI IN brwproc[5].
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FRANEXCEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FRANEXCEL DIALOG-3
ON CHOOSE OF BTN_FRANEXCEL IN FRAME FRAME-GRUND /* Importera Kontakter */
DO:
   DEFINE VARIABLE initDir AS CHARACTER NO-UNDO.
   DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.
   DEFINE VARIABLE dialogVar AS LOGICAL NO-UNDO.
   initDir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN initDir = webclienttempdir.

      SYSTEM-DIALOG GET-FILE cFile
      TITLE "Välj fil Excel fil för Import"
      FILTERS 'All Files (*.xls*)' '*.xls*'
      INITIAL-DIR initDir
      UPDATE dialogVar.
   IF dialogVar = TRUE THEN DO:
      ASSIGN
      startc = "A"
      slutc = "E"
      iRad = 1. 
      RUN readexcelstart_UI (INPUT cFile).
      DO WHILE iRad <= iRadslut:
         iRad = iRad + 1.
         RUN readexcel_UI.
         IF allacolumtext[1] NE "" THEN DO:  
            CREATE kontaktperstemp.             
            kontaktperstemp.NAMN = allacolumtext[1].
            kontaktperstemp.EPOST = allacolumtext[2].
            kontaktperstemp.TELEFON = allacolumtext[3].
            kontaktperstemp.MOBIL = allacolumtext[4].
            kontaktperstemp.FAX = allacolumtext[5]. 
         END.
      END.
      RUN slutreadexcel_UI.   
      RUN openbdyn_UI IN brwproc[5] (INPUT "").  
   END.         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY DIALOG-3
ON CHOOSE OF BTN_NY IN FRAME FRAME-GRUND /* Ny */
DO:
   CREATE kontaktperstemp.
   /*
   ASSIGN eptemp.EPOST = FILL-IN_MOTTAGARE.
   */
   RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(kontaktperstemp)).            
   RUN openbdyn_UI IN brwproc[5] (INPUT "").
   RUN lastselectdyn_UI IN brwproc[5].
   ASSIGN
   kontaktperstemp.NAMN:READ-ONLY IN BROWSE BRW_KONTAKT = FALSE   
   kontaktperstemp.EPOST:READ-ONLY IN BROWSE BRW_KONTAKT = FALSE  
   kontaktperstemp.TELEFON:READ-ONLY IN BROWSE BRW_KONTAKT = FALSE
   kontaktperstemp.MOBIL:READ-ONLY IN BROWSE BRW_KONTAKT = FALSE. 
   APPLY "ENTRY" TO kontaktperstemp.NAMN IN BROWSE BRW_KONTAKT. 
   DISPLAY BRW_KONTAKT WITH FRAME FRAME-GRUND.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DIALOG-3
&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-3
ON CHOOSE OF BTN_OK IN FRAME DIALOG-3 /* Ok */
DO:
   musz = FALSE.
   IF INPUT FRAME {&FRAME-NAME} bestkundallt.VIBESTID  = "" THEN DO:
      MESSAGE Guru.Konstanter:gbestk " får inte vara blank." VIEW-AS ALERT-BOX.        
      RETURN NO-APPLY.
   END.  
   ASSIGN 
   bestkundallt.VIBESTID    = INPUT FRAME {&FRAME-NAME} bestkundallt.VIBESTID  
   bestkundallt.BESTNAMN    = INPUT FRAME {&FRAME-NAME} bestkundallt.BESTNAMN  
   bestkundallt.KONTAKT     = INPUT FRAME FRAME-GRUND bestkundallt.KONTAKT   
   bestkundallt.TEL         = INPUT FRAME FRAME-GRUND bestkundallt.TEL       
   bestkundallt.FAXNR       = INPUT FRAME FRAME-GRUND bestkundallt.FAXNR     
   bestkundallt.ADRESS      = INPUT FRAME FRAME-GRUND bestkundallt.ADRESS    
   bestkundallt.ORT         = INPUT FRAME FRAME-GRUND bestkundallt.ORT       
   bestkundallt.PNR         = INPUT FRAME FRAME-GRUND bestkundallt.PNR       
   bestkundallt.LEVADRESS   = INPUT FRAME FRAME-GRUND bestkundallt.LEVADRESS 
   bestkundallt.LEVORT      = INPUT FRAME FRAME-GRUND bestkundallt.LEVORT    
   bestkundallt.LEVPNR      = INPUT FRAME FRAME-GRUND bestkundallt.LEVPNR    
   bestkundallt.FAKADRESS   = INPUT FRAME FRAME-GRUND bestkundallt.FAKADRESS 
   bestkundallt.FAKORT      = INPUT FRAME FRAME-GRUND bestkundallt.FAKORT    
   bestkundallt.FAKPNR      = INPUT FRAME FRAME-GRUND bestkundallt.FAKPNR    
   bestkundallt.AOKUND      = INPUT FRAME FRAME-GRUND bestkundallt.AOKUND
   FILL-IN-EPOST            = INPUT FRAME FRAME-GRUND FILL-IN-EPOST
   FILL-IN-ORG              = INPUT FRAME {&FRAME-NAME} FILL-IN-ORG
   FILL-IN-KUNDNAMN2        = INPUT FRAME {&FRAME-NAME} FILL-IN-KUNDNAMN2.    
   /*bestkundallt.KUNDPRISF   = INPUT FRAME FRAME-GRUND bestkundallt.KUNDPRISF*/.    
   IF vem = ? THEN DO:
      ASSIGN bestkundallt.BESTID = bestkundallt.VIBESTID.
   END.  
   ASSIGN   
   CMB_KOST = INPUT FRAME FRAME-REGLER CMB_KOST
   CMB_LON = INPUT CMB_LON 
   CMB_MTRL = INPUT CMB_MTRL
   CMB_OVER = INPUT CMB_OVER
   CMB_OVR = INPUT CMB_OVR
   CMB_TIM = INPUT CMB_TIM
   CMB_TRA = INPUT CMB_TRA
   CMB_KUND = INPUT CMB_KUND  
   CMB_FB = INPUT CMB_FB
   FILL-IN-MOTPART = INPUT FILL-IN-MOTPART
   FILL-IN_FRTJPA = INPUT FILL-IN_FRTJPA
   FILL-IN_MTRLPA = INPUT FILL-IN_MTRLPA
   FILL-IN_FAKINT = INPUT FILL-IN_FAKINT  
   FILL-IN_SLUT% = INPUT FILL-IN_SLUT%    
   FILL-IN_START% = INPUT FILL-IN_START%
   FILL-IN_ENTRAK = INPUT FILL-IN_ENTRAK
   FILL-IN_FLERTRAK = INPUT FILL-IN_FLERTRAK 
   FILL-IN_MIL = INPUT FILL-IN_MIL
   FILL-IN_FDAGAR = INPUT FRAME FRAME-REGLER FILL-IN_FDAGAR.
   EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "BESTEPOST"                   
   inextradatatemp.HUVUDCH = bestkundallt.BESTID
   inextradatatemp.SOKCHAR[1] = FILL-IN-EPOST
   inextradatatemp.SOKCHAR[3] = FILL-IN-ORG
   inextradatatemp.SOKCHAR[4] = FILL-IN-KUNDNAMN2. 
   IF FILL-IN-AVD = "ALLMÄN" OR FILL-IN-AVD = " " THEN inextradatatemp.SOKCHAR[2] = FILL-IN-AVD.
   ELSE IF FILL-IN-AVD = "ej kopplad" THEN inextradatatemp.SOKCHAR[2] = "".   
   ELSE DO:
      FIND FIRST avdelningtemp WHERE avdelningtemp.AVDELNINGNAMN = FILL-IN-AVD NO-LOCK NO-ERROR.
      inextradatatemp.SOKCHAR[2] = STRING(avdelningtemp.AVDELNINGNR).
   END.     
   RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp).           
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR.
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "BESTEPOST2"                   
   inextradatatemp.HUVUDCH = bestkundallt.BESTID. 
   RUN exbort_UI IN edataapph (INPUT TABLE inextradatatemp).
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR.
   FOR EACH kontaktperstemp:
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "BESTEPOST2"                   
      inextradatatemp.HUVUDCH = bestkundallt.BESTID 
      inextradatatemp.SOKCHAR[1]  =  kontaktperstemp.NAMN   
      inextradatatemp.SOKCHAR[2]  =  kontaktperstemp.EPOST  
      inextradatatemp.SOKCHAR[3]  =  kontaktperstemp.TELEFON
      inextradatatemp.SOKCHAR[4]  =  kontaktperstemp.MOBIL
      inextradatatemp.SOKCHAR[5]  =  kontaktperstemp.FAX.        
   END.  
   RUN sparaextra_UI IN edataapph (INPUT TABLE inextradatatemp). 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   IF Guru.Konstanter:varforetypval[3] >= 1 AND Guru.Konstanter:varforetypval[3] < 4 THEN DO:
      EMPTY TEMP-TABLE inextrakopptemp NO-ERROR.       
      CREATE inextrakopptemp.          
      ASSIGN
      inextrakopptemp.PROGRAM = "FBBESID"                   
      inextrakopptemp.KOPPLACHAR1 = bestkundallt.BESTID              
      inextrakopptemp.KOPPLAINT1 =  ?      
      inextrakopptemp.KOPPLACHAR2 = ?            
      inextrakopptemp.KOPPLAINT2 =  ?.   
      RUN exbort_UI IN fbestapph (INPUT TABLE inextrakopptemp).
      EMPTY TEMP-TABLE inextrakopptemp NO-ERROR.       
      IF CMB_FB = "" THEN.
      ELSE IF CMB_FB NE "Ingen" THEN DO:
         FIND FIRST fbtemp WHERE fbtemp.NAMN = CMB_FB NO-LOCK NO-ERROR.
         CREATE inextrakopptemp.          
         ASSIGN
         inextrakopptemp.PROGRAM = "FBBESID"                   
         inextrakopptemp.KOPPLACHAR1 = bestkundallt.BESTID              
         inextrakopptemp.KOPPLAINT1 =  ?      
         inextrakopptemp.KOPPLACHAR2 = fbtemp.ANV
         inextrakopptemp.KOPPLAINT2 =  ?.   
         inextrakopptemp.SOKCHAR[1] = fbtemp.ANV.
         RUN sparaextra_UI IN fbestapph (INPUT TABLE inextrakopptemp).
      END.
      EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      IF CMB_KUND = "Faktureras ej" THEN CMB_KUND = CMB_KUND.
      ELSE DO:
         FIND FIRST kundtyptemp WHERE kundtyptemp.KUNDTYP = CMB_KUND NO-LOCK NO-ERROR.   
         FIND FIRST kundregeltemp WHERE kundregeltemp.BESTID = bestkundallt.BESTID 
         NO-ERROR.
         IF NOT AVAILABLE kundregeltemp THEN DO:
            FIND FIRST kundregeltemp NO-ERROR.
         END.
      END.
      IF NOT AVAILABLE kundregeltemp THEN DO:
         FIND FIRST kundregeltemp NO-ERROR.
      END.
      
      IF FILL-IN_FAKINT = 0 THEN DO:
         MESSAGE "Faktureringsintervallet måste vara mer än 0 dagar." VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.      
      END.  
      IF CMB_LON = "Endast utvalda löne." THEN kundregeltemp.LONRGL = "ENDAST MILERSÄTTNING".
      ELSE kundregeltemp.LONRGL = CMB_LON.
      ASSIGN
      kundregeltemp.BESTID = bestkundallt.BESTID      
      kundregeltemp.KOSTREGRGL = CMB_KOST      
      /*kundregeltemp.MTRLRGL = CMB_MTRL
      kundregeltemp.OVRKRGL = CMB_OVR*/   
      kundregeltemp.OVERTIDRGL  = CMB_OVER
      kundregeltemp.SLUT% = FILL-IN_SLUT%
      kundregeltemp.START% = FILL-IN_START%      
      kundregeltemp.TIMRGL = CMB_TIM
      kundregeltemp.TRAKTRGL = CMB_TRA
      kundregeltemp.FAKINT = FILL-IN_FAKINT
      kundregeltemp.ENTRAK = FILL-IN_ENTRAK
      kundregeltemp.FLERTRAK = FILL-IN_FLERTRAK 
      kundregeltemp.MIL = FILL-IN_MIL
      kundregeltemp.FRTJPA = FILL-IN_FRTJPA
      kundregeltemp.MTRLPA = FILL-IN_MTRLPA
      kundregeltemp.FDAGAR = FILL-IN_FDAGAR.
      IF AVAILABLE kundtyptemp THEN kundregeltemp.KUNDID = kundtyptemp.KUNDID.                                      
      IF CMB_KUND = "Faktureras ej" THEN kundregeltemp.KUNDID = ?.

      FOR EACH kundbeftemp:
         kundbeftemp.BESTID = bestkundallt.BESTID.
      END.
      FOR EACH kundovertemp:
         kundovertemp.BESTID = bestkundallt.BESTID.
      END.
      FOR EACH kundprislisttemp:
         kundprislisttemp.BESTID = bestkundallt.BESTID.
      END.
   END.
   RUN bestspara_UI IN bestapph (INPUT vem,INPUT TABLE bestkundallt,INPUT TABLE kundregeltemp,INPUT TABLE kundbeftemp,
   INPUT TABLE kundovertemp,INPUT TABLE kundprislisttemp,OUTPUT TABLE felmeddtemp).                   
   FIND FIRST felmeddtemp NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
      DELETE felmeddtemp.
      RETURN NO-APPLY.
   END.  
   vem = bestkundallt.BESTID.
   IF VALID-HANDLE(fbestapph) THEN DELETE PROCEDURE fbestapph.      
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   {BORTBRWPROC.I}
   APPLY "GO" TO FRAME {&FRAME-NAME}.         
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-GRUND
&Scoped-define SELF-NAME BTN_OUTLOOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OUTLOOK DIALOG-3
ON CHOOSE OF BTN_OUTLOOK IN FRAME FRAME-GRUND /* Från Outlook */
DO:
   
   {muswait.i}  
   {AVBGOM.I}
   bestkundallt.KONTAKT = INPUT bestkundallt.KONTAKT.
   RUN EPOSTVAL.W (INPUT bestkundallt.KONTAKT ,OUTPUT epostvar).
   {AVBFRAM.I}   
   {musarrow.i}
   IF epostvar NE "" THEN DO:
      FILL-IN-EPOST = epostvar.
      DISPLAY FILL-IN-EPOST WITH FRAME {&FRAME-NAME}.      
   END.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_BESTAVD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_BESTAVD DIALOG-3
ON VALUE-CHANGED OF CMB_BESTAVD IN FRAME FRAME-GRUND /* Avdelning */
DO:
  CMB_BESTAVD = INPUT CMB_BESTAVD.
  FILL-IN-AVD = CMB_BESTAVD.
  /*IF  CMB_BESTAVD = "Ej kopplad" THEN FILL-IN-AVD = "".*/
  DISPLAY FILL-IN-AVD WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-REGLER
&Scoped-define SELF-NAME CMB_KUND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_KUND DIALOG-3
ON ENTRY OF CMB_KUND IN FRAME FRAME-REGLER /* Kundtyp */
DO:
   RUN tabstop_UI (INPUT TRUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-GRUND
&Scoped-define SELF-NAME FILL-IN-EPOST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-EPOST DIALOG-3
ON LEAVE OF FILL-IN-EPOST IN FRAME FRAME-GRUND /* E-post */
DO:
   FILL-IN-EPOST = INPUT FILL-IN-EPOST.
   IF FILL-IN-EPOST NE "" THEN DO:   
      musz = FALSE.
      RUN EPOSTKOLL.P (INPUT FILL-IN-EPOST,OUTPUT musz).   
      IF musz = FALSE THEN DO:
         APPLY "ENTRY" TO FILL-IN-EPOST IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.         
      END.      
      musz = FALSE.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-REGLER
&Scoped-define SELF-NAME FILL-IN-MOTPART
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-MOTPART DIALOG-3
ON ENTRY OF FILL-IN-MOTPART IN FRAME FRAME-REGLER /* Motpart */
DO:
   RUN tabstop_UI (INPUT TRUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-MOTPART DIALOG-3
ON LEAVE OF FILL-IN-MOTPART IN FRAME FRAME-REGLER /* Motpart */
DO:
   
   FILL-IN-MOTPART = INPUT FILL-IN-MOTPART.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_MTRLPA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_MTRLPA DIALOG-3
ON ENTRY OF FILL-IN_MTRLPA IN FRAME FRAME-REGLER /* Materiel i % */
DO:
   RUN tabstop_UI (INPUT TRUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DIALOG-3
&Scoped-define SELF-NAME RAD_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAL DIALOG-3
ON VALUE-CHANGED OF RAD_VAL IN FRAME DIALOG-3
DO:
   RAD_VAL = INPUT RAD_VAL.
   /*RAD_VAL:ADD-LAST("Fakt.regler", 2) IN FRAME {&FRAME-NAME}.
   RAD_VAL:ADD-LAST("Kundpriser", 3).
   RAD_VAL:ADD-LAST("Övertidspriser", 4).
   */
   
   ASSIGN
   FRAME FRAME-REGLER:VISIBLE = FALSE
   FRAME FRAME-OVER:VISIBLE = FALSE
   FRAME FRAME-PRISBEF:VISIBLE = FALSE
   FRAME FRAME-GRUND:VISIBLE = FALSE.
   IF RAD_VAL = 1 THEN DO:
      ASSIGN
      FRAME FRAME-GRUND:VISIBLE = TRUE.      
   /*   bestkundallt.KUNDPRISF:HIDDEN IN FRAME FRAME-GRUND = TRUE.*/
   END.
   IF RAD_VAL = 2 THEN DO:
      ASSIGN
      FRAME FRAME-REGLER:VISIBLE = TRUE.      
      IF fbok = TRUE THEN DO:
         ENABLE CMB_FB WITH FRAME FRAME-REGLER.
         DISPLAY CMB_FB WITH FRAME FRAME-REGLER.
      END.
   END.
   IF RAD_VAL = 3 THEN DO:
      ASSIGN
      FRAME FRAME-PRISBEF:VISIBLE = TRUE.
      brwsortvar = 'defpristemp.OVER = FALSE'.
      RUN setcolsortvar_UI IN brwproc[2] (INPUT brwsortvar).
      RUN openbdynspec_UI IN brwproc[2].
      FIND FIRST kundprislisttemp WHERE kundprislisttemp.BESTID = bestkundallt.BESTID AND 
      kundprislisttemp.OVER = FALSE
      NO-ERROR.
      IF AVAILABLE kundprislisttemp THEN DO:
         FIND FIRST defpristemp WHERE defpristemp.PRISID = kundprislisttemp.PRISID AND
         defpristemp.OVER = FALSE
         NO-LOCK NO-ERROR.
         IF AVAILABLE defpristemp THEN DO:
            RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(defpristemp)).              
            RUN lastselectdyn_UI IN brwproc[2].
            defrow[2] = ROWID(defpristemp).
            BRW_BEF:TITLE IN FRAME FRAME-PRISBEF = defpristemp.PRISTEXT + " startdatum " + STRING(kundprislisttemp.STARTDATUM).
         END.
      END.
      
   END.
   IF RAD_VAL = 4 THEN DO:
      ASSIGN
      FRAME FRAME-OVER:VISIBLE = TRUE.
      brwsortvar = 'defpristemp.OVER = TRUE'.
      RUN setcolsortvar_UI IN brwproc[3] (INPUT brwsortvar).
      RUN openbdynspec_UI IN brwproc[3].         
      FIND FIRST kundprislisttemp WHERE kundprislisttemp.BESTID = bestkundallt.BESTID AND 
      kundprislisttemp.OVER = TRUE
      NO-LOCK NO-ERROR.
      IF AVAILABLE kundprislisttemp THEN DO:
         FIND FIRST defpristemp WHERE defpristemp.PRISID = kundprislisttemp.PRISID AND
         defpristemp.OVER = TRUE NO-LOCK NO-ERROR.
         IF AVAILABLE defpristemp THEN DO:
            RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(defpristemp)).              
            RUN lastselectdyn_UI IN brwproc[3].  
            defrow[3] = ROWID(defpristemp).
            BRW_KUNDO:TITLE  IN FRAME FRAME-OVER = defpristemp.PRISTEXT + " startdatum " + STRING(kundprislisttemp.STARTDATUM).
         END.
      END.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME bestkundallt.VIBESTID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL bestkundallt.VIBESTID DIALOG-3
ON LEAVE OF bestkundallt.VIBESTID IN FRAME DIALOG-3 /* Best-id */
DO:
   IF INPUT bestkundallt.VIBESTID = "" THEN DO:
      MESSAGE Guru.Konstanter:gbestk "kan inte vara blank!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF vem = ? THEN DO:
      IF bestkundallt.VIBESTID NE INPUT bestkundallt.VIBESTID THEN DO:
         bestkundallt.VIBESTID = INPUT bestkundallt.VIBESTID.
         RUN namnkoll_UI IN bestapph (INPUT vem,INPUT bestkundallt.VIBESTID,OUTPUT TABLE felmeddtemp).                   
         FIND FIRST felmeddtemp NO-ERROR.
         IF AVAILABLE felmeddtemp THEN DO:
            MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
            DELETE felmeddtemp.
            RETURN NO-APPLY.
         END.  
      END.
   END.
   bestkundallt.VIBESTID = INPUT bestkundallt.VIBESTID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_BEF
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-3 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Add Trigger to equate WINDOW-CLOSE to END-ERROR                      */
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {DIA_M_START.I}      
   {ALLSTARTDYN.I}
   RUN hamtaen_UI IN bestapph (INPUT vem,OUTPUT TABLE bestkundallt).    
   /*
   RUN fixepost_UI IN bestapph (INPUT vem).
   */
   RUN hamtaenextra_UI IN bestapph (INPUT vem,OUTPUT TABLE bestkundextra).                   
   IF vem = ? THEN RUN autobestid_UI IN bestapph (OUTPUT autovem).
   CMB_FB = "Ingen".
   FOR EACH avdelningtemp:
      status-ok = CMB_BESTAVD:ADD-LAST(avdelningtemp.AVDELNINGNAMN).
   END.
   FIND FIRST bestkundallt NO-ERROR.
   IF vem = ? THEN DO:
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ny " + Guru.Konstanter:gbestk.   
      CMB_BESTAVD:SCREEN-VALUE = "Allmän".
      CMB_BESTAVD = "Allmän".
      FILL-IN-AVD = CMB_BESTAVD.
      bestkundallt.VIBESTID = autovem.
   END.
   ELSE DO:
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ändra-" + bestkundallt.BESTNAMN.      
      EMPTY TEMP-TABLE extradatatemp NO-ERROR.       
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "BESTEPOST"                   
      inextradatatemp.HUVUDCH = bestkundallt.BESTID.                   
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FIND FIRST extradatatemp NO-LOCK NO-ERROR.
      IF AVAILABLE extradatatemp THEN DO:      
         ASSIGN
         FILL-IN-EPOST = extradatatemp.SOKCHAR[1]
         FILL-IN-ORG   = extradatatemp.SOKCHAR[3] 
         FILL-IN-KUNDNAMN2 =extradatatemp.SOKCHAR[4]. 
         IF extradatatemp.SOKCHAR[2] = "ALLMÄN" THEN FILL-IN-AVD = extradatatemp.SOKCHAR[2].
         ELSE IF extradatatemp.SOKCHAR[2] = "" THEN FILL-IN-AVD = "Ej kopplad".                  
         ELSE DO:
            FIND FIRST avdelningtemp WHERE avdelningtemp.AVDELNINGNR = INTEGER(extradatatemp.SOKCHAR[2])  NO-LOCK NO-ERROR.
            FILL-IN-AVD = avdelningtemp.AVDELNINGNAMN.
         END.
         CMB_BESTAVD = FILL-IN-AVD.      
         CMB_BESTAVD:SCREEN-VALUE = FILL-IN-AVD.         
      END.                                      
      IF FILL-IN-ORG = "" THEN FILL-IN-ORG = "0000000000".
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      EMPTY TEMP-TABLE extradatatemp NO-ERROR.
      CREATE inextradatatemp.          
      ASSIGN
      inextradatatemp.PROGRAM = "BESTEPOST2"                   
      inextradatatemp.HUVUDCH = bestkundallt.BESTID.                    
      RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
      FOR EACH extradatatemp:
         CREATE kontaktperstemp.
         ASSIGN
         kontaktperstemp.NAMN    = extradatatemp.SOKCHAR[1]
         kontaktperstemp.EPOST   = extradatatemp.SOKCHAR[2]
         kontaktperstemp.TELEFON = extradatatemp.SOKCHAR[3]
         kontaktperstemp.MOBIL   = extradatatemp.SOKCHAR[4]
         kontaktperstemp.FAX   = extradatatemp.SOKCHAR[5].
      END.
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      EMPTY TEMP-TABLE extradatatemp NO-ERROR.       



   END.
   
   IF Guru.Konstanter:varforetypval[3] >= 1 AND Guru.Konstanter:varforetypval[3] < 4 THEN DO:   
      RUN faktupp_UI.                         
   END.
   
   /*CMB_BESTAVD:DELIMITER = "$". */
   /*status-ok = CMB_BESTAVD:ADD-LAST(gavdk + " : alla").*/
   /*
   status-ok = CMB_BESTAVD:ADD-LAST("Ej kopplad").
   status-ok = CMB_BESTAVD:ADD-LAST("Allmän").
   */
        
   CMB_BESTAVD:LABEL = Guru.Konstanter:gavdl.
   FILL-IN-AVD:LABEL = "Tillhör " + Guru.Konstanter:gavdl.
   RUN enable_UI.
   
   RAD_VAL = 1.
   DISABLE FILL-IN-AVD WITH FRAME FRAME-GRUND.
   /*{FRMSIZED.I}*/
   {FRMSIZEDF.I}
    ASSIGN                                                          
    kontaktperstemp.NAMN:READ-ONLY IN BROWSE BRW_KONTAKT = TRUE    
    kontaktperstemp.EPOST:READ-ONLY IN BROWSE BRW_KONTAKT = TRUE   
    kontaktperstemp.TELEFON:READ-ONLY IN BROWSE BRW_KONTAKT = TRUE 
    kontaktperstemp.FAX:READ-ONLY IN BROWSE BRW_KONTAKT = TRUE
    kontaktperstemp.MOBIL:READ-ONLY IN BROWSE BRW_KONTAKT = TRUE.  
      
   ASSIGN
   bestkundallt.VIBESTID:LABEL = Guru.Konstanter:gbestk    
   bestkundallt.AOKUND:LABEL = "Koppling till " + LC(Guru.Konstanter:gaok).   
   APPLY "VALUE-CHANGED" TO RAD_VAL.
   IF Guru.Konstanter:varforetypval[3] >= 1 AND Guru.Konstanter:varforetypval[3] < 4 THEN DO:
      DISPLAY RAD_VAL WITH FRAME {&FRAME-NAME}.
   END.
   ELSE RAD_VAL:HIDDEN = TRUE.
   IF Guru.Konstanter:globforetag = "cAKEA" THEN DO: 
      RAD_VAL:ADD-LAST("Kundpriser", 3).
      DISPLAY RAD_VAL WITH FRAME {&FRAME-NAME}.
   END. 
   RUN setapplyvc IN framesizeh (INPUT RAD_VAL:HANDLE).
   /*bestkundallt.KUNDPRISF:HIDDEN IN FRAME FRAME-GRUND = TRUE.*/
   BTN_OUTLOOK:HIDDEN = TRUE.
   {musarrow.i}
   {DIA_M_SLUT.I}          
   APPLY "ENTRY" TO bestkundallt.VIBESTID.
   RUN openbdyn_UI IN brwproc[5] (INPUT "").
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI DIALOG-3 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   ASSIGN
   kundbeftemp.NAMN:READ-ONLY IN BROWSE BRW_BEF = TRUE    
   defpristemp.PRISNUM:READ-ONLY IN BROWSE BRW_DPRISBEF = TRUE
   defpristemp.PRISNUM:READ-ONLY IN BROWSE BRW_DPRISOVER = TRUE
   kundovertemp.OTEXT:READ-ONLY IN BROWSE BRW_KUNDO = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_BEF:HANDLE IN FRAME FRAME-PRISBEF).
   RUN DYNBRW.P PERSISTENT SET brwproc[2] 
      (INPUT BRW_DPRISBEF:HANDLE IN FRAME FRAME-PRISBEF).
   RUN DYNBRW.P PERSISTENT SET brwproc[3] 
      (INPUT BRW_DPRISOVER:HANDLE IN FRAME FRAME-OVER).
   RUN DYNBRW.P PERSISTENT SET brwproc[4] 
      (INPUT BRW_KUNDO:HANDLE IN FRAME FRAME-OVER).
   RUN DYNBRW.P PERSISTENT SET brwproc[5] 
      (INPUT BRW_KONTAKT:HANDLE IN FRAME FRAME-GRUND).
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRATABHMT.P PERSISTENT SET fbestapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRATABHMT.P PERSISTENT SET fbestapph.      
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
   END.
   RUN mouseselclick_UI IN brwproc[2] (INPUT FALSE). /*Disable APPLY "VALUE-CHANGED" TO browser*/
   RUN mouseselclick_UI IN brwproc[3] (INPUT FALSE).
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-3  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME DIALOG-3.
  HIDE FRAME FRAME-GRUND.
  HIDE FRAME FRAME-OVER.
  HIDE FRAME FRAME-PRISBEF.
  HIDE FRAME FRAME-REGLER.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-3  _DEFAULT-ENABLE
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
  DISPLAY RAD_VAL FILL-IN-KUNDNAMN2 FILL-IN-ORG 
      WITH FRAME DIALOG-3.
  IF AVAILABLE bestkundallt THEN 
    DISPLAY bestkundallt.VIBESTID bestkundallt.BESTNAMN 
      WITH FRAME DIALOG-3.
  ENABLE RAD_VAL bestkundallt.VIBESTID bestkundallt.BESTNAMN FILL-IN-KUNDNAMN2 
         FILL-IN-ORG BTN_OK BTN_AVB 
      WITH FRAME DIALOG-3.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-3}
  DISPLAY FILL-IN-AVD CMB_BESTAVD FILL-IN-EPOST 
      WITH FRAME FRAME-GRUND.
  IF AVAILABLE bestkundallt THEN 
    DISPLAY bestkundallt.KONTAKT bestkundallt.TEL bestkundallt.FAXNR 
          bestkundallt.ADRESS bestkundallt.PNR bestkundallt.ORT 
          bestkundallt.LEVADRESS bestkundallt.LEVPNR bestkundallt.LEVORT 
          bestkundallt.FAKADRESS bestkundallt.FAKPNR bestkundallt.FAKORT 
          bestkundallt.AOKUND 
      WITH FRAME FRAME-GRUND.
  ENABLE BTN_FRANEXCEL BTN_OUTLOOK FILL-IN-AVD CMB_BESTAVD bestkundallt.KONTAKT 
         BRW_KONTAKT bestkundallt.TEL bestkundallt.FAXNR FILL-IN-EPOST 
         bestkundallt.ADRESS bestkundallt.PNR bestkundallt.ORT 
         bestkundallt.LEVADRESS bestkundallt.LEVPNR bestkundallt.LEVORT 
         bestkundallt.FAKADRESS bestkundallt.FAKPNR bestkundallt.FAKORT 
         bestkundallt.AOKUND BTN_NY BTN_ANDRA BTN_BORT 
      WITH FRAME FRAME-GRUND.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-GRUND}
  ENABLE BRW_DPRISOVER BRW_KUNDO 
      WITH FRAME FRAME-OVER.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-OVER}
  ENABLE BRW_DPRISBEF BRW_BEF 
      WITH FRAME FRAME-PRISBEF.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-PRISBEF}
  DISPLAY CMB_KUND FILL-IN_FDAGAR FILL-IN_FAKINT CMB_TIM CMB_KOST CMB_OVER 
          CMB_LON CMB_TRA FILL-IN_ENTRAK FILL-IN_FLERTRAK FILL-IN_MIL 
          FILL-IN_START% FILL-IN_SLUT% FILL-IN_FRTJPA FILL-IN_MTRLPA 
      WITH FRAME FRAME-REGLER.
  ENABLE CMB_KUND FILL-IN_FDAGAR FILL-IN_FAKINT CMB_TIM CMB_KOST CMB_OVER 
         CMB_LON CMB_TRA FILL-IN_ENTRAK FILL-IN_FLERTRAK FILL-IN_MIL 
         FILL-IN_START% FILL-IN_SLUT% FILL-IN_FRTJPA FILL-IN_MTRLPA 
      WITH FRAME FRAME-REGLER.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-REGLER}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE faktupp_UI DIALOG-3 
PROCEDURE faktupp_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   status-ok = CMB_KUND:DELETE("CCC") IN FRAME FRAME-REGLER NO-ERROR.
  DEBUGGER:SET-BREAK().
   RUN fakupphmt_UI IN bestapph (INPUT bestkundallt.BESTID,OUTPUT TABLE kundtyptemp,
   OUTPUT TABLE kundregeltemp,OUTPUT TABLE kundbeftemp,OUTPUT TABLE defpristemp, 
   OUTPUT TABLE kundovertemp, OUTPUT TABLE kundprislisttemp,OUTPUT TABLE felmeddtemp).                   
   FIND FIRST felmeddtemp NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
      DELETE felmeddtemp.
      RETURN.
   END.       
   RAD_VAL:ADD-LAST("Fakt.regler", 2) IN FRAME {&FRAME-NAME}.
   RAD_VAL:ADD-LAST("Kundpriser", 3).
   RAD_VAL:ADD-LAST("Övertidspriser", 4).
   RUN openbdyn_UI IN brwproc[1] (INPUT "").

   RUN setpdatacolsort_UI IN brwproc[4] (INPUT "OTEXT",INPUT "kundovertemp.OTEXT BY kundovertemp.BEFATTNING BY kundovertemp.EQDAG BY kundovertemp.START
   BY kundovertemp.SLUT").
   RUN setpdatacolsort_UI IN brwproc[4] (INPUT "DAGTYP",INPUT "kundovertemp.DAGTYP BY kundovertemp.BEFATTNING BY kundovertemp.OTEXT BY kundovertemp.EQDAG BY kundovertemp.START
   BY kundovertemp.SLUT").
   RUN setpdatacolsort_UI IN brwproc[4] (INPUT "START",INPUT "kundovertemp.START BY kundovertemp.BEFATTNING BY kundovertemp.OTEXT BY kundovertemp.EQDAG 
   BY kundovertemp.SLUT").
   RUN setpdatacolsort_UI IN brwproc[4] (INPUT "SLUT",INPUT "kundovertemp.SLUT BY kundovertemp.BEFATTNING BY kundovertemp.OTEXT BY kundovertemp.EQDAG BY kundovertemp.START").
   RUN setpdatacolsort_UI IN brwproc[4] (INPUT "PRISA",INPUT "kundovertemp.PRISA BY kundovertemp.BEFATTNING BY kundovertemp.OTEXT BY kundovertemp.EQDAG BY kundovertemp.START
   BY kundovertemp.SLUT").
   RUN openbdynspec_UI IN brwproc[4].
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "cSNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
      OPEN QUERY ktq FOR EACH kundtyptemp WHERE kundtyptemp.KUNDTYP = "Intern" NO-LOCK.
   END.
   ELSE IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
      OPEN QUERY ktq FOR EACH kundtyptemp WHERE kundtyptemp.KUNDTYP = "Intern" OR kundtyptemp.KUNDTYP = "Strökund" NO-LOCK.
   END.
   ELSE DO:
      OPEN QUERY ktq FOR EACH kundtyptemp NO-LOCK.
   END.
   GET FIRST ktq NO-LOCK.
   DO WHILE AVAILABLE(kundtyptemp):
      status-ok = CMB_KUND:ADD-LAST(kundtyptemp.KUNDTYP).
      CMB_KUND:SCREEN-VALUE = kundtyptemp.KUNDTYP.
      GET NEXT ktq NO-LOCK.
   END.   
   CMB_KUND:ADD-LAST("Faktureras ej").
    
   RUN laddkreg_UI.     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE laddkreg_UI DIALOG-3 
PROCEDURE laddkreg_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND FIRST kundregeltemp WHERE kundregeltemp.BESTID = bestkundallt.BESTID 
   NO-LOCK NO-ERROR.
   FIND FIRST kundtyptemp WHERE kundtyptemp.KUNDID = kundregeltemp.KUNDID NO-LOCK NO-ERROR.
   IF AVAILABLE kundtyptemp THEN CMB_KUND:SCREEN-VALUE IN FRAME FRAME-REGLER = kundtyptemp.KUNDTYP.           
   ELSE DO:
      CMB_KUND:SCREEN-VALUE IN FRAME FRAME-REGLER = "Faktureras ej".                
   END.
   IF kundregeltemp.LONRGL = "ENDAST MILERSÄTTNING" THEN CMB_LON:SCREEN-VALUE = "Endast utvalda löne.". 
   ELSE DO:
      IF kundregeltemp.LONRGL = "" THEN kundregeltemp.LONRGL = "Inga".
      CMB_LON:SCREEN-VALUE = kundregeltemp.LONRGL. 
      CMB_LON = kundregeltemp.LONRGL. 
   END.
   ASSIGN     
   CMB_KOST:SCREEN-VALUE = kundregeltemp.KOSTREGRGL    
   CMB_MTRL:SCREEN-VALUE = kundregeltemp.MTRLRGL 
   CMB_OVER:SCREEN-VALUE = kundregeltemp.OVERTIDRGL 
   CMB_OVR:SCREEN-VALUE = kundregeltemp.OVRKRGL 
   CMB_TIM:SCREEN-VALUE = kundregeltemp.TIMRGL 
   CMB_TRA:SCREEN-VALUE = kundregeltemp.TRAKTRGL 
   CMB_KOST = kundregeltemp.KOSTREGRGL    
   CMB_MTRL = kundregeltemp.MTRLRGL 
   CMB_OVER = kundregeltemp.OVERTIDRGL 
   CMB_OVR  = kundregeltemp.OVRKRGL 
   CMB_TIM  = kundregeltemp.TIMRGL 
   CMB_TRA  = kundregeltemp.TRAKTRGL 
   FILL-IN_ENTRAK = kundregeltemp.ENTRAK
   FILL-IN_FLERTRAK = kundregeltemp.FLERTRAK 
   FILL-IN_MIL = kundregeltemp.MIL   
   FILL-IN_FAKINT =  kundregeltemp.FAKINT 
   FILL-IN_SLUT% = kundregeltemp.SLUT%        
   FILL-IN_START% = kundregeltemp.START%
   FILL-IN_FRTJPA = kundregeltemp.FRTJPA
   FILL-IN_MTRLPA = kundregeltemp.MTRLPA
   FILL-IN_FDAGAR = kundregeltemp.FDAGAR.    
   RUN fakturabesthmtalla_UI IN fbestapph (OUTPUT TABLE extrakopptemp).
   FIND FIRST extrakopptemp NO-ERROR.
   IF AVAILABLE extrakopptemp THEN DO:
      FOR EACH extrakopptemp BY extrakopptemp.SOKCHAR[1]:
         IF extrakopptemp.SOKCHAR[1] NE "" THEN DO:           
            CREATE fbtemp.
            ASSIGN 
            fbtemp.NAMN = extrakopptemp.SOKCHAR[2]
            fbtemp.ANV = extrakopptemp.KOPPLACHAR2.
            status-ok = CMB_FB:ADD-LAST(extrakopptemp.SOKCHAR[2]).
         END.         
      END.

      CREATE inextrakopptemp.          
      ASSIGN
      inextrakopptemp.PROGRAM = "FBBESID"                   
      inextrakopptemp.KOPPLACHAR1 = kundregeltemp.BESTID              
      inextrakopptemp.KOPPLAINT1 =  ?      
      inextrakopptemp.KOPPLACHAR2 = ?            
      inextrakopptemp.KOPPLAINT2 =  ?.
      RUN etabhamt_UI IN fbestapph (INPUT TABLE inextrakopptemp,OUTPUT TABLE extrakopptemp).        
      FIND FIRST extrakopptemp NO-ERROR.
      IF AVAILABLE extrakopptemp THEN DO:         
         FIND FIRST fbtemp WHERE fbtemp.ANV = extrakopptemp.KOPPLACHAR2 NO-LOCK NO-ERROR.
         IF AVAILABLE fbtemp THEN DO:
            CMB_FB:SCREEN-VALUE = fbtemp.NAMN NO-ERROR.
         END.
         ELSE CMB_FB:SCREEN-VALUE = "Ingen".       
      END.
      ELSE CMB_FB:SCREEN-VALUE = "Ingen". 
      fbok = TRUE.           
   END.
   ELSE fbok = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nylista_UI DIALOG-3 
PROCEDURE nylista_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {muswait.i}      
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   /*
   MESSAGE "Är du säker på att du byta prislista. Allt gammalt kommer att tas bort"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE defpristemp.PRISTEXT
   UPDATE answer AS LOGICAL.
   IF answer = FALSE THEN DO:
      musz = TRUE.
      RETURN NO-APPLY.
   END.
   */
   IF vad = 1 THEN DO:
      FOR EACH kundprislisttemp WHERE kundprislisttemp.BESTID = bestkundallt.BESTID AND 
      kundprislisttemp.OVER = FALSE:
         DELETE kundprislisttemp. 
      END.
      CREATE kundprislisttemp.
      ASSIGN
      kundprislisttemp.BESTID = bestkundallt.BESTID
      kundprislisttemp.PRISID = defpristemp.PRISID
      kundprislisttemp.OVER = FALSE
      kundprislisttemp.STARTDATUM = TODAY.      
      FIND FIRST kundregeltemp WHERE kundregeltemp.BESTID = bestkundallt.BESTID 
      NO-ERROR.
      CMB_TIM = INPUT FRAME FRAME-REGLER CMB_TIM.
      ASSIGN      
      kundregeltemp.TIMRGL = CMB_TIM
      kundregeltemp.ENTRAK   =  FILL-IN_ENTRAK  
      kundregeltemp.FLERTRAK =   FILL-IN_FLERTRAK
      kundregeltemp.FRTJPA   =   FILL-IN_FRTJPA  
      kundregeltemp.MIL      =   FILL-IN_MIL     
      kundregeltemp.MTRLPA  =   FILL-IN_MTRLPA.  
      RUN nypris_UI IN bestapph (INPUT bestkundallt.BESTID,INPUT defpristemp.PRISID,INPUT-OUTPUT TABLE kundregeltemp,OUTPUT TABLE kundbeftemp).       
      FIND FIRST kundregeltemp WHERE kundregeltemp.BESTID = bestkundallt.BESTID 
      NO-ERROR.
      IF AVAILABLE kundregeltemp THEN DO:
         ASSIGN
         FILL-IN_ENTRAK = kundregeltemp.ENTRAK
         FILL-IN_FLERTRAK = kundregeltemp.FLERTRAK 
         FILL-IN_FRTJPA = kundregeltemp.FRTJPA
         FILL-IN_MIL = kundregeltemp.MIL   
         FILL-IN_MTRLPA = kundregeltemp.MTRLPA.  
         DISPLAY FILL-IN_ENTRAK FILL-IN_FLERTRAK FILL-IN_FRTJPA FILL-IN_MIL FILL-IN_MTRLPA WITH FRAME FRAME-REGLER. 
      END.
      RUN openbdyn_UI IN brwproc[1] (INPUT "").
   END.
   IF vad = 2 THEN DO:
      FOR EACH kundprislisttemp WHERE kundprislisttemp.BESTID = bestkundallt.BESTID AND 
      kundprislisttemp.OVER = TRUE:
         DELETE kundprislisttemp. 
      END.
      CREATE kundprislisttemp.
      ASSIGN
      kundprislisttemp.BESTID = bestkundallt.BESTID
      kundprislisttemp.PRISID = defpristemp.PRISID
      kundprislisttemp.OVER = TRUE
      kundprislisttemp.STARTDATUM = TODAY.      
      RUN nypriso_UI IN bestapph (INPUT bestkundallt.BESTID,INPUT defpristemp.PRISID,OUTPUT TABLE kundovertemp).
      RUN openbdynspec_UI IN brwproc[4].
   END.
   APPLY "VALUE-CHANGED" TO RAD_VAL IN FRAME {&FRAME-NAME}.
   {musarrow.i}
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tabstop_UI DIALOG-3 
PROCEDURE tabstop_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER tabstop AS LOGICAL NO-UNDO.
   ASSIGN
   BTN_OK:TAB-STOP IN FRAME {&FRAME-NAME} = tabstop
   BTN_AVB:TAB-STOP IN FRAME {&FRAME-NAME} = tabstop.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION tabortblanka DIALOG-3 
FUNCTION tabortblanka RETURNS CHARACTER
  ( INPUT namnblanka AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

 namnblanka = REPLACE(namnblanka," ","").
  RETURN   namnblanka.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

