&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame


/* Temp-Table and Buffer definitions                                    */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
&Scoped-define NEW                          
{FAKTTEMP.I}
{FAKTPLANTEMP.I}
DEFINE INPUT PARAMETER kreditvar AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER infakplannr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER fdelnrvar AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER rundavar AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER varfaktypnr AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER frirow AS ROWID NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER nya AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER knappvar AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR faktaonrtemp.
DEFINE INPUT PARAMETER TABLE FOR faktfriatemp.
DEFINE VARIABLE entrymtrlantal AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE regmannamn AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE regmnr AS INTEGER FORMAT "99" NO-UNDO.
DEFINE SHARED VARIABLE regar AS INTEGER FORMAT "99" NO-UNDO.
   /* Local Variable Definitions ---                                       */

{ALLDEF.I}
{ANMARKTEMP.I}

{GLOBVAR2DEL1.I}
{FAKKOTEMP.I}
{MTRLTEMP.I}
{HUVLEVTEMP.I}
{LEVTEMP.I}
{DEFSOK.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{SOKDEF.I}
DEFINE NEW SHARED VARIABLE anmarkapph AS HANDLE NO-UNDO.
DEFINE NEW SHARED VARIABLE fakkoproch AS HANDLE NO-UNDO. /* FAKKAPP.P */
DEFINE SHARED VARIABLE fakthmth AS HANDLE NO-UNDO.
DEFINE VARIABLE laddaproch AS HANDLE NO-UNDO.
DEFINE VARIABLE mtrlhmtapph AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE vartyp AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE frinr AS INTEGER NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE posok AS CHARACTER FORMAT "X(11)" NO-UNDO.
DEFINE VARIABLE sok AS LOGICAL NO-UNDO.  
DEFINE VARIABLE brow AS ROWID NO-UNDO.
DEFINE VARIABLE pbrow AS ROWID NO-UNDO.
DEFINE VARIABLE anmarvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER NO-UNDO.
DEFINE VARIABLE multiaosok AS CHARACTER NO-UNDO.
/*DEFINE VARIABLE wh AS WIDGET-HANDLE.*/
DEFINE VARIABLE sortvar AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO. 
DEFINE VARIABLE vald_kundlev AS CHARACTER NO-UNDO.
DEFINE VARIABLE vald_lev AS CHARACTER NO-UNDO.  
DEFINE VARIABLE offert AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE fakantalvar AS DECIMAL NO-UNDO. 
DEFINE VARIABLE fakoantalvar AS DECIMAL NO-UNDO. 
DEFINE VARIABLE fakoprisvar AS DECIMAL NO-UNDO. 
DEFINE VARIABLE fakprisvar AS DECIMAL NO-UNDO.
DEFINE VARIABLE fnr AS INTEGER  NO-UNDO.
DEFINE VARIABLE fdel AS INTEGER NO-UNDO.
DEFINE VARIABLE vtyp AS CHARACTER NO-UNDO.
DEFINE VARIABLE faktja AS LOGICAL NO-UNDO.


DEFINE TEMP-TABLE beftemp
  FIELD BEFATTNING AS CHARACTER
  FIELD NAMN AS CHARACTER.
DEFINE TEMP-TABLE obeftemp
  FIELD BEFATTNING AS CHARACTER
  FIELD OTEXT AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BRW_ANM

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES fastanmtemp kalkkattemp mtrltemp momstemp ~
faktmtrltemp ekalkkattemp

/* Definitions for BROWSE BRW_ANM                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_ANM fastanmtemp.ANVANDARE ~
fastanmtemp.ANMARK 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ANM fastanmtemp.ANVANDARE 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_ANM fastanmtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_ANM fastanmtemp
&Scoped-define QUERY-STRING-BRW_ANM FOR EACH fastanmtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ANM OPEN QUERY BRW_ANM FOR EACH fastanmtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ANM fastanmtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ANM fastanmtemp


/* Definitions for BROWSE BRW_FRIP                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_FRIP kalkkattemp.VINAMN kalkkattemp.PRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_FRIP 
&Scoped-define QUERY-STRING-BRW_FRIP FOR EACH kalkkattemp NO-LOCK ~
    BY mtrltemp.Enr
&Scoped-define OPEN-QUERY-BRW_FRIP OPEN QUERY BRW_FRIP FOR EACH kalkkattemp NO-LOCK ~
    BY mtrltemp.Enr.
&Scoped-define TABLES-IN-QUERY-BRW_FRIP kalkkattemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_FRIP kalkkattemp


/* Definitions for BROWSE BRW_HLEV                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_HLEV mtrltemp.Enr mtrltemp.Benamning ~
mtrltemp.Enhet mtrltemp.NPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_HLEV mtrltemp.Enr 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_HLEV mtrltemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_HLEV mtrltemp
&Scoped-define QUERY-STRING-BRW_HLEV FOR EACH mtrltemp NO-LOCK ~
    BY mtrltemp.Enr
&Scoped-define OPEN-QUERY-BRW_HLEV OPEN QUERY BRW_HLEV FOR EACH mtrltemp NO-LOCK ~
    BY mtrltemp.Enr.
&Scoped-define TABLES-IN-QUERY-BRW_HLEV mtrltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_HLEV mtrltemp


/* Definitions for BROWSE BRW_K4                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_K4 momstemp.MOMSKOD momstemp.MOMSTEXT ~
momstemp.MOMSEXTERNT momstemp.MOMSNR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_K4 
&Scoped-define QUERY-STRING-BRW_K4 FOR EACH momstemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_K4 OPEN QUERY BRW_K4 FOR EACH momstemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_K4 momstemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_K4 momstemp


/* Definitions for BROWSE BRW_MTRL                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL faktmtrltemp.BERKVANT ~
faktmtrltemp.NPRIS faktmtrltemp.Enr faktmtrltemp.Benamning ~
faktmtrltemp.Enhet 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL faktmtrltemp.BERKVANT ~
faktmtrltemp.NPRIS 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_MTRL faktmtrltemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_MTRL faktmtrltemp
&Scoped-define QUERY-STRING-BRW_MTRL FOR EACH faktmtrltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MTRL OPEN QUERY BRW_MTRL FOR EACH faktmtrltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL faktmtrltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL faktmtrltemp


/* Definitions for BROWSE BRW_VFRIP                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_VFRIP ekalkkattemp.VINAMN ~
ekalkkattemp.ANTAL ekalkkattemp.PRIS ekalkkattemp.TOTFAKT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VFRIP ekalkkattemp.VINAMN ~
ekalkkattemp.ANTAL ekalkkattemp.PRIS 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_VFRIP ekalkkattemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_VFRIP ekalkkattemp
&Scoped-define QUERY-STRING-BRW_VFRIP FOR EACH ekalkkattemp NO-LOCK ~
    BY ekalkkattemp.Enr
&Scoped-define OPEN-QUERY-BRW_VFRIP OPEN QUERY BRW_VFRIP FOR EACH ekalkkattemp NO-LOCK ~
    BY ekalkkattemp.Enr.
&Scoped-define TABLES-IN-QUERY-BRW_VFRIP ekalkkattemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VFRIP ekalkkattemp


/* Definitions for DIALOG-BOX Dialog-Frame                              */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS faktfriatemp.TOTALT faktfriatemp.FAKTURERAD 
&Scoped-define ENABLED-TABLES faktfriatemp
&Scoped-define FIRST-ENABLED-TABLE faktfriatemp
&Scoped-Define ENABLED-OBJECTS TOG_OFFERT FBTN_SPARA CMB_AONR TOG_DIFF ~
BRW_HLEV BRW_VFRIP BRW_FRIP BRW_K4 BRW_ANM FBTN_OK RAD_SOK BTN_AVB 
&Scoped-Define DISPLAYED-FIELDS faktfriatemp.TOTKALK faktfriatemp.TOTALT ~
faktfriatemp.FAKTURERAD 
&Scoped-define DISPLAYED-TABLES faktfriatemp
&Scoped-define FIRST-DISPLAYED-TABLE faktfriatemp
&Scoped-Define DISPLAYED-OBJECTS RAD_VAL TOG_OFFERT CMB_AONR FILL-IN-fritot ~
TOG_DIFF RAD_SOK 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD runda Dialog-Frame 
FUNCTION runda RETURNS DECIMAL
  ( INPUT varedin AS DECIMAL)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON btn_back 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 5.5 BY 1.75.

DEFINE BUTTON BTN_BORTANM 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.13.

DEFINE BUTTON BTN_KONTO 
     IMAGE-UP FILE "BILDER\upp-u":U
     LABEL "UPP":L 
     SIZE 7 BY 2.

DEFINE BUTTON BTN_NYANM 
     LABEL "Ny":L 
     SIZE 12 BY 1.13.

DEFINE BUTTON btn_over 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 5.5 BY 1.75.

DEFINE BUTTON BTN_UPPANM 
     LABEL "Ändra":L 
     SIZE 12 BY 1.13.

DEFINE BUTTON FBTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON FBTN_SPARA 
     LABEL "Spara" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE CMB_AONR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Aonr Delnr" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "CCC" 
     DROP-DOWN-LIST
     SIZE 15.5 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_LEV AS CHARACTER FORMAT "X(15)":U 
     LABEL "Leverantörer" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18.25 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_VAL AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kategori" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "CCC" 
     DROP-DOWN-LIST
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(40)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 27 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENR AS CHARACTER FORMAT "X(11)":U 
     LABEL "Enr" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-fritot AS DECIMAL FORMAT "->>,>>9.999":U INITIAL 0 
     LABEL "Total summa fria poster" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-OVRK AS CHARACTER FORMAT "X(256)":U INITIAL "Övriga kostnader" 
     VIEW-AS FILL-IN 
     SIZE 16.5 BY .88 NO-UNDO.

DEFINE VARIABLE FILL-IN_MOMSEXTERNT AS DECIMAL FORMAT ">>9.99" INITIAL 0 
     LABEL "Momssats" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_MOMSKOD AS CHARACTER FORMAT "X(8)" 
     LABEL "Momskonto" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1
     FONT 17.

DEFINE VARIABLE FILL-IN_MOMSNR AS CHARACTER FORMAT "X(8)" 
     LABEL "Momskod" 
     VIEW-AS FILL-IN 
     SIZE 9 BY 1.

DEFINE VARIABLE FILL-IN_SFORNAMN AS CHARACTER FORMAT "x(40)" 
     LABEL "Övr." 
     VIEW-AS FILL-IN 
     SIZE 16.88 BY .83.

DEFINE VARIABLE FILL-IN_SPERSONALKOD AS CHARACTER FORMAT "x(12)" 
     LABEL "Användare" 
     VIEW-AS FILL-IN 
     SIZE 13.75 BY .83 NO-UNDO.

DEFINE VARIABLE RAD_SOK AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Början", 1,
"Någonstans", 2,
"Slutet", 3
     SIZE 51.5 BY .83 NO-UNDO.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Personal", 1,
"Traktamente mm.", 3,
"Mil/Resor", 4,
"Främmande tj.", 2,
"Materiel", 5,
"Övrigt", 6
     SIZE 106.38 BY 1.08 NO-UNDO.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 55.38 BY 3.63
     BGCOLOR 8 .

DEFINE VARIABLE TOG_DIFF AS LOGICAL INITIAL no 
     LABEL "Differentierad moms" 
     VIEW-AS TOGGLE-BOX
     SIZE 25.25 BY .88 NO-UNDO.

DEFINE VARIABLE TOG_OFFERT AS LOGICAL INITIAL no 
     LABEL "Offert" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.13 BY .88 NO-UNDO.

DEFINE VARIABLE TOG_OVER AS LOGICAL INITIAL no 
     LABEL "Övertid" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.63 BY .88 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_ANM FOR 
      fastanmtemp SCROLLING.

DEFINE QUERY BRW_FRIP FOR 
      kalkkattemp SCROLLING.

DEFINE QUERY BRW_HLEV FOR 
      mtrltemp SCROLLING.

DEFINE QUERY BRW_K4 FOR 
      momstemp SCROLLING.

DEFINE QUERY BRW_MTRL FOR 
      faktmtrltemp SCROLLING.

DEFINE QUERY BRW_VFRIP FOR 
      ekalkkattemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_ANM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ANM Dialog-Frame _STRUCTURED
  QUERY BRW_ANM NO-LOCK DISPLAY
      fastanmtemp.ANVANDARE FORMAT "x(12)":U
      fastanmtemp.ANMARK FORMAT "X(50)":U
  ENABLE
      fastanmtemp.ANVANDARE
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 68.5 BY 5.04
         TITLE "Fasta anmärkningar".

DEFINE BROWSE BRW_FRIP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_FRIP Dialog-Frame _STRUCTURED
  QUERY BRW_FRIP NO-LOCK DISPLAY
      kalkkattemp.VINAMN COLUMN-LABEL "Fakt.text" FORMAT "X(256)":U
            WIDTH 30
      kalkkattemp.PRIS COLUMN-LABEL "Pris" FORMAT "->>>>>9.999":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 45.63 BY 16.71
         TITLE "Fri prislista".

DEFINE BROWSE BRW_HLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_HLEV Dialog-Frame _STRUCTURED
  QUERY BRW_HLEV NO-LOCK DISPLAY
      mtrltemp.Enr FORMAT "X(11)":U
      mtrltemp.Benamning FORMAT "x(20)":U WIDTH 15
      mtrltemp.Enhet FORMAT "x(5)":U
      mtrltemp.NPRIS FORMAT ">>>>>9.99":U
  ENABLE
      mtrltemp.Enr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 46 BY 7.75
         TITLE "Materiellista vald leverantör.".

DEFINE BROWSE BRW_K4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_K4 Dialog-Frame _STRUCTURED
  QUERY BRW_K4 NO-LOCK DISPLAY
      momstemp.MOMSKOD COLUMN-LABEL "Moms!konto" FORMAT "X(5)":U
      momstemp.MOMSTEXT COLUMN-LABEL "Text" FORMAT "X(40)":U
      momstemp.MOMSEXTERNT COLUMN-LABEL "Moms!i procent" FORMAT ">>9.99":U
            WIDTH 8.88
      momstemp.MOMSNR COLUMN-LABEL "Moms!kod" FORMAT "X(4)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 64.13 BY 10.88
         TITLE "Momstabell".

DEFINE BROWSE BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL Dialog-Frame _STRUCTURED
  QUERY BRW_MTRL NO-LOCK DISPLAY
      faktmtrltemp.BERKVANT COLUMN-LABEL "Antal" FORMAT "->>>>9.99":U
      faktmtrltemp.NPRIS FORMAT ">>>>>9.99":U
      faktmtrltemp.Enr FORMAT "X(11)":U
      faktmtrltemp.Benamning FORMAT "x(20)":U WIDTH 17.13
      faktmtrltemp.Enhet FORMAT "x(5)":U
  ENABLE
      faktmtrltemp.BERKVANT
      faktmtrltemp.NPRIS
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 59.5 BY 8
         TITLE "Vald materiel".

DEFINE BROWSE BRW_VFRIP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VFRIP Dialog-Frame _STRUCTURED
  QUERY BRW_VFRIP NO-LOCK DISPLAY
      ekalkkattemp.VINAMN COLUMN-LABEL "Fakt.text" FORMAT "X(256)":U
            WIDTH 30.5
      ekalkkattemp.ANTAL COLUMN-LABEL "Antal" FORMAT "->>>>>>9.99<":U
      ekalkkattemp.PRIS COLUMN-LABEL "Pris" FORMAT "->>>>>9.999":U
      ekalkkattemp.TOTFAKT COLUMN-LABEL "Tot/post" FORMAT "->,>>>,>>9":U
  ENABLE
      ekalkkattemp.VINAMN
      ekalkkattemp.ANTAL
      ekalkkattemp.PRIS
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 57 BY 13
         TITLE "Fri prislista".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     RAD_VAL AT ROW 1.25 COL 1.5 NO-LABEL
     TOG_OFFERT AT ROW 2.38 COL 1.5
     CMB_VAL AT ROW 3.54 COL 11.38 COLON-ALIGNED
     faktfriatemp.FAKTTEXT AT ROW 3.58 COL 11.38 COLON-ALIGNED FORMAT "X(256)"
          VIEW-AS FILL-IN 
          SIZE 41 BY 1
     TOG_OVER AT ROW 3.67 COL 60.13
     faktfriatemp.ANTAL AT ROW 4.83 COL 11.38 COLON-ALIGNED WIDGET-ID 2 FORMAT "->>>>>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     faktfriatemp.PRIS_ENHET AT ROW 4.83 COL 38.38 COLON-ALIGNED FORMAT "->>>>>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     faktfriatemp.OANTAL AT ROW 6 COL 11.38 COLON-ALIGNED FORMAT "->>>>>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     faktfriatemp.OPRIS AT ROW 6 COL 38.38 COLON-ALIGNED FORMAT "->>>>>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     faktfriatemp.TOTKALK AT ROW 7.17 COL 11.38 COLON-ALIGNED FORMAT "->>>>>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     faktfriatemp.TOTALT AT ROW 7.17 COL 38.38 COLON-ALIGNED FORMAT "->>>>>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 13 BY 1
     FBTN_SPARA AT ROW 8 COL 111.38
     faktfriatemp.FAKTURERAD AT ROW 8.42 COL 11.38 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     CMB_AONR AT ROW 8.42 COL 38.38 COLON-ALIGNED
     FILL-IN-fritot AT ROW 9.96 COL 79 COLON-ALIGNED WIDGET-ID 4
     FILL-IN_MOMSEXTERNT AT ROW 10 COL 43.5 COLON-ALIGNED
     FILL-IN_MOMSNR AT ROW 10 COL 60.88 COLON-ALIGNED
     FILL-IN_MOMSKOD AT ROW 10 COL 81.88 COLON-ALIGNED
     TOG_DIFF AT ROW 10.04 COL 1.5
     BRW_HLEV AT ROW 11.54 COL 1.5
     BRW_VFRIP AT ROW 11.54 COL 53.38
     BRW_MTRL AT ROW 11.54 COL 53.5
     BRW_FRIP AT ROW 11.58 COL 1.5
     BRW_K4 AT ROW 12.67 COL 23.38
     BTN_KONTO AT ROW 13.08 COL 40.25
     btn_over AT ROW 14 COL 47.63
     BRW_ANM AT ROW 15.75 COL 10.63
     btn_back AT ROW 17.5 COL 47.63
     CMB_LEV AT ROW 20.33 COL 16.63 COLON-ALIGNED
     BTN_NYANM AT ROW 21.25 COL 21.88
     BTN_UPPANM AT ROW 21.25 COL 37.75
     BTN_BORTANM AT ROW 21.25 COL 53.63
     FILL-IN-OVRK AT ROW 22.88 COL 16.88 COLON-ALIGNED NO-LABEL
     FILL-IN-ENR AT ROW 24.79 COL 34.38 COLON-ALIGNED
     FILL-IN_SPERSONALKOD AT ROW 24.88 COL 27.88 COLON-ALIGNED
     FILL-IN_SFORNAMN AT ROW 24.88 COL 48.38 COLON-ALIGNED
     FILL-IN-BEN AT ROW 26.04 COL 34.25 COLON-ALIGNED
     FBTN_OK AT ROW 26.13 COL 111.38
     RAD_SOK AT ROW 27.13 COL 18 NO-LABEL
     BTN_AVB AT ROW 27.21 COL 111.38
     RECT-21 AT ROW 24.58 COL 16
     SPACE(54.61) SKIP(1.16)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Fri komplettering".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Temp-Tables and Buffers:
      TABLE: ekalkkattemp T "?" NO-UNDO temp-db ekalkkattemp
      TABLE: faktfriatemp T "?" NO-UNDO temp-db faktfriatemp
      TABLE: faktmtrltemp T "?" NO-UNDO temp-db faktmtrltemp
      TABLE: fastanmtemp T "?" NO-UNDO temp-db fastanmtemp
      TABLE: kalkkattemp T "?" NO-UNDO temp-db kalkkattemp
      TABLE: momstemp T "?" NO-UNDO temp-db momstemp
      TABLE: mtrltemp T "?" NO-UNDO temp-db mtrltemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BRW_HLEV TOG_DIFF Dialog-Frame */
/* BROWSE-TAB BRW_VFRIP BRW_HLEV Dialog-Frame */
/* BROWSE-TAB BRW_MTRL BRW_VFRIP Dialog-Frame */
/* BROWSE-TAB BRW_FRIP BRW_MTRL Dialog-Frame */
/* BROWSE-TAB BRW_K4 BRW_FRIP Dialog-Frame */
/* BROWSE-TAB BRW_ANM btn_over Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN faktfriatemp.ANTAL IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE EXP-FORMAT                                      */
ASSIGN 
       faktfriatemp.ANTAL:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       BRW_ANM:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

ASSIGN 
       BRW_FRIP:HIDDEN  IN FRAME Dialog-Frame                = TRUE
       BRW_FRIP:MAX-DATA-GUESS IN FRAME Dialog-Frame         = 50000
       BRW_FRIP:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE
       BRW_FRIP:COLUMN-RESIZABLE IN FRAME Dialog-Frame       = TRUE.

ASSIGN 
       BRW_HLEV:HIDDEN  IN FRAME Dialog-Frame                = TRUE
       BRW_HLEV:MAX-DATA-GUESS IN FRAME Dialog-Frame         = 50000
       BRW_HLEV:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

ASSIGN 
       BRW_K4:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

/* SETTINGS FOR BROWSE BRW_MTRL IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       BRW_MTRL:HIDDEN  IN FRAME Dialog-Frame                = TRUE
       BRW_MTRL:MAX-DATA-GUESS IN FRAME Dialog-Frame         = 40000
       BRW_MTRL:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

ASSIGN 
       BRW_VFRIP:HIDDEN  IN FRAME Dialog-Frame                = TRUE
       BRW_VFRIP:MAX-DATA-GUESS IN FRAME Dialog-Frame         = 50000
       BRW_VFRIP:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE
       BRW_VFRIP:COLUMN-RESIZABLE IN FRAME Dialog-Frame       = TRUE.

/* SETTINGS FOR BUTTON btn_back IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BTN_BORTANM IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       BTN_BORTANM:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON BTN_KONTO IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       BTN_KONTO:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON BTN_NYANM IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       BTN_NYANM:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR BUTTON btn_over IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BTN_UPPANM IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       BTN_UPPANM:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_LEV IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       CMB_LEV:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR COMBO-BOX CMB_VAL IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       CMB_VAL:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN faktfriatemp.FAKTTEXT IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE EXP-FORMAT                                      */
ASSIGN 
       faktfriatemp.FAKTTEXT:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-BEN IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-BEN:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-ENR IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-ENR:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-fritot IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-OVRK IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-OVRK:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_MOMSEXTERNT IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_MOMSEXTERNT:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_MOMSKOD IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_MOMSKOD:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_MOMSNR IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_MOMSNR:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_SFORNAMN IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_SFORNAMN:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_SPERSONALKOD IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN_SPERSONALKOD:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN faktfriatemp.OANTAL IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE EXP-FORMAT                                      */
ASSIGN 
       faktfriatemp.OANTAL:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN faktfriatemp.OPRIS IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE EXP-FORMAT                                      */
ASSIGN 
       faktfriatemp.OPRIS:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN faktfriatemp.PRIS_ENHET IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE EXP-FORMAT                                      */
ASSIGN 
       faktfriatemp.PRIS_ENHET:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR RADIO-SET RAD_VAL IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR RECTANGLE RECT-21 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       RECT-21:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR TOGGLE-BOX TOG_OVER IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TOG_OVER:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN faktfriatemp.TOTALT IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN faktfriatemp.TOTKALK IN FRAME Dialog-Frame
   NO-ENABLE EXP-FORMAT                                                 */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ANM
/* Query rebuild information for BROWSE BRW_ANM
     _TblList          = "Temp-Tables.fastanmtemp"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.fastanmtemp.ANVANDARE
"fastanmtemp.ANVANDARE" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.fastanmtemp.ANMARK
"fastanmtemp.ANMARK" ? "X(50)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_ANM */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_FRIP
/* Query rebuild information for BROWSE BRW_FRIP
     _TblList          = "Temp-Tables.kalkkattemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.mtrltemp.Enr|yes"
     _FldNameList[1]   > Temp-Tables.kalkkattemp.VINAMN
"kalkkattemp.VINAMN" "Fakt.text" "X(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.kalkkattemp.PRIS
"kalkkattemp.PRIS" "Pris" "->>>>>9.999" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_FRIP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_HLEV
/* Query rebuild information for BROWSE BRW_HLEV
     _TblList          = "Temp-Tables.mtrltemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.mtrltemp.Enr|yes"
     _FldNameList[1]   > Temp-Tables.mtrltemp.Enr
"mtrltemp.Enr" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.mtrltemp.Benamning
"mtrltemp.Benamning" ? "x(20)" "character" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.mtrltemp.Enhet
     _FldNameList[4]   > Temp-Tables.mtrltemp.NPRIS
"mtrltemp.NPRIS" ? ">>>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_HLEV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_K4
/* Query rebuild information for BROWSE BRW_K4
     _TblList          = "Temp-Tables.momstemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.momstemp.MOMSKOD
"momstemp.MOMSKOD" "Moms!konto" "X(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.momstemp.MOMSTEXT
"momstemp.MOMSTEXT" "Text" "X(40)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.momstemp.MOMSEXTERNT
"momstemp.MOMSEXTERNT" "Moms!i procent" ? "decimal" ? ? ? ? ? ? no ? no no "8.88" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.momstemp.MOMSNR
"momstemp.MOMSNR" "Moms!kod" "X(4)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_K4 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRL
/* Query rebuild information for BROWSE BRW_MTRL
     _TblList          = "Temp-Tables.faktmtrltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.faktmtrltemp.BERKVANT
"faktmtrltemp.BERKVANT" "Antal" "->>>>9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.faktmtrltemp.NPRIS
"faktmtrltemp.NPRIS" ? ">>>>>9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = Temp-Tables.faktmtrltemp.Enr
     _FldNameList[4]   > Temp-Tables.faktmtrltemp.Benamning
"faktmtrltemp.Benamning" ? "x(20)" "character" ? ? ? ? ? ? no ? no no "17.13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = Temp-Tables.faktmtrltemp.Enhet
     _Query            is NOT OPENED
*/  /* BROWSE BRW_MTRL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VFRIP
/* Query rebuild information for BROWSE BRW_VFRIP
     _TblList          = "Temp-Tables.ekalkkattemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.ekalkkattemp.Enr|yes"
     _FldNameList[1]   > Temp-Tables.ekalkkattemp.VINAMN
"ekalkkattemp.VINAMN" "Fakt.text" "X(256)" "character" ? ? ? ? ? ? yes ? no no "30.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.ekalkkattemp.ANTAL
"ekalkkattemp.ANTAL" "Antal" "->>>>>>9.99<" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.ekalkkattemp.PRIS
"ekalkkattemp.PRIS" "Pris" "->>>>>9.999" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.ekalkkattemp.TOTFAKT
"ekalkkattemp.TOTFAKT" "Tot/post" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_VFRIP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Fri komplettering */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Fri komplettering */
DO:  
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME faktfriatemp.ANTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktfriatemp.ANTAL Dialog-Frame
ON LEAVE OF faktfriatemp.ANTAL IN FRAME Dialog-Frame /* ANTAL */
DO:
   RUN entryantal_UI.
   RUN summa_UI.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_FRIP
&Scoped-define SELF-NAME BRW_FRIP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_FRIP Dialog-Frame
ON ANY-KEY OF BRW_FRIP IN FRAME Dialog-Frame /* Fri prislista */
DO:
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "CHOOSE" TO BTN_OVER.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_FRIP Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF BRW_FRIP IN FRAME Dialog-Frame /* Fri prislista */
DO:
   APPLY "CHOOSE" TO BTN_OVER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_HLEV
&Scoped-define SELF-NAME BRW_HLEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_HLEV Dialog-Frame
ON ANY-KEY OF BRW_HLEV IN FRAME Dialog-Frame /* Materiellista vald leverantör. */
DO:
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "CHOOSE" TO BTN_OVER.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_HLEV Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF BRW_HLEV IN FRAME Dialog-Frame /* Materiellista vald leverantör. */
DO:
   APPLY "CHOOSE" TO BTN_OVER.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_K4
&Scoped-define SELF-NAME BRW_K4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K4 Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF BRW_K4 IN FRAME Dialog-Frame /* Momstabell */
DO:
   /*RUN andra_UI.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_K4 Dialog-Frame
ON VALUE-CHANGED OF BRW_K4 IN FRAME Dialog-Frame /* Momstabell */
DO:
   ASSIGN
   FILL-IN_MOMSEXTERNT = momstemp.MOMSEXTERNT
   FILL-IN_MOMSKOD     = momstemp.MOMSKOD    
   FILL-IN_MOMSNR      = momstemp.MOMSNR.
   DISPLAY FILL-IN_MOMSEXTERNT FILL-IN_MOMSKOD FILL-IN_MOMSNR WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_MTRL
&Scoped-define SELF-NAME BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_MTRL Dialog-Frame
ON ANY-KEY OF BRW_MTRL IN FRAME Dialog-Frame /* Vald materiel */
DO:
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "CHOOSE" TO BTN_BACK.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME faktmtrltemp.BERKVANT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktmtrltemp.BERKVANT BRW_MTRL _BROWSE-COLUMN Dialog-Frame
ON ENTRY OF faktmtrltemp.BERKVANT IN BROWSE BRW_MTRL /* Antal */
DO:
   DISPLAY faktmtrltemp.BERKVANT WITH BROWSE BRW_MTRL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktmtrltemp.BERKVANT BRW_MTRL _BROWSE-COLUMN Dialog-Frame
ON LEAVE OF faktmtrltemp.BERKVANT IN BROWSE BRW_MTRL /* Antal */
DO:
   DEFINE VARIABLE gpris AS DECIMAL.
   gpris = faktmtrltemp.NPRIS * faktmtrltemp.BERKVANT.    
   faktmtrltemp.BERKVANT = INPUT BROWSE BRW_MTRL faktmtrltemp.BERKVANT.    
   faktfriatemp.PRIS_ENHET = faktfriatemp.PRIS_ENHET - gpris.
   faktfriatemp.PRIS_ENHET = 
   faktfriatemp.PRIS_ENHET + faktmtrltemp.NPRIS * faktmtrltemp.BERKVANT.
   DISPLAY faktmtrltemp.BERKVANT WITH BROWSE BRW_MTRL.
   DISPLAY faktfriatemp.PRIS_ENHET WITH FRAME {&FRAME-NAME}.
   RUN summa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME faktmtrltemp.NPRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktmtrltemp.NPRIS BRW_MTRL _BROWSE-COLUMN Dialog-Frame
ON ENTRY OF faktmtrltemp.NPRIS IN BROWSE BRW_MTRL /* Netto pris */
DO:
    DISPLAY faktmtrltemp.NPRIS WITH BROWSE BRW_MTRL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktmtrltemp.NPRIS BRW_MTRL _BROWSE-COLUMN Dialog-Frame
ON LEAVE OF faktmtrltemp.NPRIS IN BROWSE BRW_MTRL /* Netto pris */
DO:
   DEFINE VARIABLE gpris AS DECIMAL.
   gpris = faktmtrltemp.NPRIS * faktmtrltemp.BERKVANT.    
   faktmtrltemp.NPRIS = INPUT BROWSE BRW_MTRL faktmtrltemp.NPRIS.    
   faktfriatemp.PRIS_ENHET = faktfriatemp.PRIS_ENHET - gpris.
   faktfriatemp.PRIS_ENHET = faktfriatemp.PRIS_ENHET + faktmtrltemp.NPRIS * faktmtrltemp.BERKVANT.
   DISPLAY faktmtrltemp.NPRIS WITH BROWSE BRW_MTRL.
   DISPLAY faktfriatemp.PRIS_ENHET WITH FRAME {&FRAME-NAME}.
   RUN summa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VFRIP
&Scoped-define SELF-NAME BRW_VFRIP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VFRIP Dialog-Frame
ON ANY-KEY OF BRW_VFRIP IN FRAME Dialog-Frame /* Fri prislista */
DO:
   IF KEYFUNCTION(LASTKEY) = ("END-ERROR") THEN  RETURN NO-APPLY. 
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "CHOOSE" TO BTN_OVER.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VFRIP Dialog-Frame
ON ROW-LEAVE OF BRW_VFRIP IN FRAME Dialog-Frame /* Fri prislista */
DO:
   IF AVAILABLE ekalkkattemp THEN DO:
      DISPLAY ekalkkattemp.VINAMN ekalkkattemp.ANTAL ekalkkattemp.PRIS WITH BROWSE BRW_VFRIP.
      ASSIGN
      ekalkkattemp.VINAMN = INPUT BROWSE BRW_VFRIP ekalkkattemp.VINAMN
      ekalkkattemp.ANTAL = INPUT BROWSE BRW_VFRIP ekalkkattemp.ANTAL 
      ekalkkattemp.PRIS = INPUT BROWSE BRW_VFRIP ekalkkattemp.PRIS.     
      RUN fritot_UI.
         
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VFRIP Dialog-Frame
ON VALUE-CHANGED OF BRW_VFRIP IN FRAME Dialog-Frame /* Fri prislista */
DO:
   status-ok = BRW_VFRIP:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ekalkkattemp.VINAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ekalkkattemp.VINAMN BRW_VFRIP _BROWSE-COLUMN Dialog-Frame
ON ENTRY OF ekalkkattemp.VINAMN IN BROWSE BRW_VFRIP /* Fakt.text */
DO:
   IF AVAILABLE ekalkkattemp THEN DO:
      DISPLAY ekalkkattemp.VINAMN ekalkkattemp.ANTAL ekalkkattemp.PRIS WITH BROWSE BRW_VFRIP.     
      RUN fritot_UI. 
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ekalkkattemp.VINAMN BRW_VFRIP _BROWSE-COLUMN Dialog-Frame
ON LEAVE OF ekalkkattemp.VINAMN IN BROWSE BRW_VFRIP /* Fakt.text */
DO:
   IF AVAILABLE ekalkkattemp THEN DO:
      ASSIGN
      ekalkkattemp.VINAMN = INPUT BROWSE BRW_VFRIP ekalkkattemp.VINAMN.
      DISPLAY ekalkkattemp.VINAMN ekalkkattemp.ANTAL ekalkkattemp.PRIS WITH BROWSE BRW_VFRIP.      
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ekalkkattemp.ANTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ekalkkattemp.ANTAL BRW_VFRIP _BROWSE-COLUMN Dialog-Frame
ON ENTRY OF ekalkkattemp.ANTAL IN BROWSE BRW_VFRIP /* Antal */
DO:
   entrymtrlantal = TRUE.
   IF AVAILABLE ekalkkattemp THEN DO:
      DISPLAY ekalkkattemp.VINAMN ekalkkattemp.ANTAL ekalkkattemp.PRIS WITH BROWSE BRW_VFRIP.      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ekalkkattemp.ANTAL BRW_VFRIP _BROWSE-COLUMN Dialog-Frame
ON LEAVE OF ekalkkattemp.ANTAL IN BROWSE BRW_VFRIP /* Antal */
DO:
   IF entrymtrlantal = TRUE THEN DO:
      IF AVAILABLE ekalkkattemp THEN DO:
         ASSIGN
         ekalkkattemp.ANTAL = INPUT BROWSE BRW_VFRIP ekalkkattemp.ANTAL. 
         ekalkkattemp.TOTFAKT = ekalkkattemp.ANTAL * ekalkkattemp.PRIS.      
         DISPLAY ekalkkattemp.VINAMN ekalkkattemp.ANTAL ekalkkattemp.PRIS ekalkkattemp.TOTFAKT WITH BROWSE BRW_VFRIP.    
         RUN fritot_UI.  
      END.
   END.
   entrymtrlantal = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ekalkkattemp.PRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ekalkkattemp.PRIS BRW_VFRIP _BROWSE-COLUMN Dialog-Frame
ON ENTRY OF ekalkkattemp.PRIS IN BROWSE BRW_VFRIP /* Pris */
DO:
   IF AVAILABLE ekalkkattemp THEN DO:
      DISPLAY ekalkkattemp.VINAMN ekalkkattemp.ANTAL ekalkkattemp.PRIS WITH BROWSE BRW_VFRIP.      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ekalkkattemp.PRIS BRW_VFRIP _BROWSE-COLUMN Dialog-Frame
ON LEAVE OF ekalkkattemp.PRIS IN BROWSE BRW_VFRIP /* Pris */
DO:
   IF AVAILABLE ekalkkattemp THEN DO:
      ASSIGN
      ekalkkattemp.PRIS = INPUT BROWSE BRW_VFRIP ekalkkattemp.PRIS.
      ekalkkattemp.TOTFAKT = ekalkkattemp.ANTAL * ekalkkattemp.PRIS.
      DISPLAY ekalkkattemp.VINAMN ekalkkattemp.ANTAL ekalkkattemp.PRIS ekalkkattemp.TOTFAKT WITH BROWSE BRW_VFRIP.   
      RUN fritot_UI.   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB Dialog-Frame
ON CHOOSE OF BTN_AVB IN FRAME Dialog-Frame /* Avbryt */
DO:
   RUN entryantal_UI.
   IF nya = FALSE THEN DO:
      faktfriatemp.FAKTURERAD = faktja.
      IF faktfriatemp.FAKTURERAD = TRUE THEN DO:
         FIND FIRST gfaktemp WHERE gfaktemp.ORDNING = 3 NO-ERROR.
         
         gfaktemp.TOTALT = gfaktemp.TOTALT + faktfriatemp.TOTALT.
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
         IF faktfriatemp.TYP = "OVER" THEN DO:
            gfaktemp.OVRIG = gfaktemp.OVRIG + faktfriatemp.TOTALT.
         END.
         IF faktfriatemp.TYP = "MATRL" THEN DO:
            gfaktemp.MTRL = gfaktemp.MTRL + faktfriatemp.TOTALT.
         END.
         IF rundavar = TRUE THEN DO:                         
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
         END.
      END.
   END.
   knappvar = 1. 
   IF VALID-HANDLE(mtrlhmtapph) THEN DELETE PROCEDURE mtrlhmtapph.
   IF VALID-HANDLE(fakkoproch) THEN DELETE PROCEDURE fakkoproch. 
   IF VALID-HANDLE(laddaproch) THEN DELETE PROCEDURE laddaproch.
   IF VALID-HANDLE(tthandle) THEN DELETE OBJECT tthandle.
   IF VALID-HANDLE(anmarkapph) THEN DELETE PROCEDURE anmarkapph.
   {BORTBRWPROC.I}
   APPLY "GO" TO BTN_AVB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB Dialog-Frame
ON GO OF BTN_AVB IN FRAME Dialog-Frame /* Avbryt */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_back
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_back Dialog-Frame
ON CHOOSE OF btn_back IN FRAME Dialog-Frame
DO: 
   RUN entryantal_UI.
   IF RAD_VAL = 5 THEN DO:
      antal_valda = BRW_MTRL:NUM-SELECTED-ROWS NO-ERROR.
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:                                   
         status-ok = BRW_MTRL:FETCH-SELECTED-ROW(antal_raknare) NO-ERROR.                      
         IF AVAILABLE faktmtrltemp THEN DO:
            faktfriatemp.PRIS_ENHET = faktfriatemp.PRIS_ENHET - faktmtrltemp.NPRIS * faktmtrltemp.BERKVANT.   
            DISPLAY faktfriatemp.PRIS_ENHET WITH FRAME {&FRAME-NAME}.
            RUN summa_UI.
            DELETE faktmtrltemp.         
         END.
         antal_raknare = antal_raknare + 1.   
      END.
      RUN refreshbrw_UI IN brwproc[3].
   END.
   IF RAD_VAL = 7 THEN DO:
      antal_valda = BRW_VFRIP:NUM-SELECTED-ROWS NO-ERROR.
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:                                   
         status-ok = BRW_VFRIP:FETCH-SELECTED-ROW(antal_raknare) NO-ERROR.                               
         IF AVAILABLE ekalkkattemp THEN DO:
            IF ekalkkattemp.ROWFRIA NE ? THEN musz = musz.
            ELSE DELETE ekalkkattemp.         
         END.
         antal_raknare = antal_raknare + 1.   
      END.
      RUN refreshbrw_UI IN brwproc[6].
      RUN fritot_UI.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORTANM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORTANM Dialog-Frame
ON CHOOSE OF BTN_BORTANM IN FRAME Dialog-Frame /* Ta bort */
DO:
   RUN entryantal_UI.
   RUN bortanm_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_KONTO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_KONTO Dialog-Frame
ON CHOOSE OF BTN_KONTO IN FRAME Dialog-Frame /* UPP */
DO:
   RUN entryantal_UI.
   RUN anm_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NYANM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NYANM Dialog-Frame
ON CHOOSE OF BTN_NYANM IN FRAME Dialog-Frame /* Ny */
DO:
   RUN entryantal_UI.
   RUN ny_UI.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_over
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_over Dialog-Frame
ON CHOOSE OF btn_over IN FRAME Dialog-Frame
DO: 
   DEFINE VARIABLE monthhelp AS INTEGER NO-UNDO.
   RUN entryantal_UI.
   IF RAD_VAL = 5 THEN DO:
      antal_valda = BRW_HLEV:NUM-SELECTED-ROWS NO-ERROR.
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:                                   
         status-ok = BRW_HLEV:FETCH-SELECTED-ROW(antal_raknare) NO-ERROR.
         RUN nyfaktmtrl_UI.
         antal_raknare = antal_raknare + 1.
         RUN setlastrowid_UI IN brwproc[3] (INPUT ROWID(faktmtrltemp)).
      END.      
      RUN openbdyn_UI IN brwproc[3] (INPUT "").   
      RUN lastselectdyn_UI IN brwproc[3].
   END.
   IF RAD_VAL = 7 THEN DO:
      DEFINE VARIABLE extradag AS INTEGER NO-UNDO.
      antal_valda = BRW_FRIP:NUM-SELECTED-ROWS NO-ERROR.
      antal_raknare = 1.
      DO WHILE antal_raknare LE antal_valda:                                   
         status-ok = BRW_FRIP:FETCH-SELECTED-ROW(antal_raknare) NO-ERROR.
         CREATE ekalkkattemp.
         BUFFER-COPY kalkkattemp TO ekalkkattemp.
         ekalkkattemp.TOTFAKT = ekalkkattemp.ANTAL * ekalkkattemp.PRIS.
         RUN fritot_UI. 
         /*mallorca mallis*/
         IF TODAY = 06/30/21 THEN DO:
            extradag = 1.
            
         END.    
         ELSE DO:
            extradag = 0.
         END.   
         FIND FIRST GuruGrundPrisTT WHERE GuruGrundPrisTT.NAMN = "G0Plan" + STRING(faktfriatemp.FAKTNR) NO-LOCK NO-ERROR.
         IF NOT AVAILABLE GuruGrundPrisTT THEN DO:
            IF faktfriatemp.FAKTNR >= 90 THEN DO:
               FIND FIRST GuruGrundPrisTT WHERE GuruGrundPrisTT.NAMN = "G2Över90" NO-LOCK NO-ERROR.
            END.   
            ELSE DO:
               FIND FIRST GuruGrundPrisTT WHERE GuruGrundPrisTT.NAMN = "G1Under90" NO-LOCK NO-ERROR.
            END.
         END.
         IF ekalkkattemp.VINAMN = "Guru imånad" THEN DO:
            regmnr = MONTH(TODAY + extradag).
            RUN MANNAMN.P.
          /*   ekalkkattemp.PRIS = 1500  IF faktfriatemp.FAKTNR >= 90 THEN ekalkkattemp.PRIS = 1650.          */
            ASSIGN 
            ekalkkattemp.ANTAL = 1
            ekalkkattemp.VINAMN = "Guru " + regmannamn.
         END.
         ELSE IF ekalkkattemp.VINAMN = "Guru månadslicens imånad" THEN DO:
            regmnr = MONTH(TODAY + extradag).
            RUN MANNAMN.P.
            /*  IF faktfriatemp.FAKTNR >= 90 THEN ekalkkattemp.PRIS = 2050.       ekalkkattemp.PRIS = 1900*/
            FIND FIRST GuruGrundPrisTT WHERE GuruGrundPrisTT.NAMN = "G0Plan" + STRING(faktfriatemp.FAKTNR) + "M" NO-LOCK NO-ERROR.
            IF NOT AVAILABLE GuruGrundPrisTT THEN DO:
               IF faktfriatemp.FAKTNR >= 90 THEN DO:
                  FIND FIRST GuruGrundPrisTT WHERE GuruGrundPrisTT.NAMN = "G2Över90M" NO-LOCK NO-ERROR.
               END.   
               ELSE DO:
                  FIND FIRST GuruGrundPrisTT WHERE GuruGrundPrisTT.NAMN = "G1Under90M" NO-LOCK NO-ERROR.
               END.
            END.
            ASSIGN
            ekalkkattemp.ANTAL = 1
            ekalkkattemp.VINAMN = "Guru månadslicens " + regmannamn.
         END.
         ELSE IF ekalkkattemp.VINAMN = "Guru år" THEN DO:
            regmnr = MONTH(TODAY + extradag).
            RUN MANNAMN.P.
            ekalkkattemp.ANTAL = 12.
            ekalkkattemp.VINAMN = "Guru " + STRING(YEAR(TODAY  + extradag)) + STRING(MONTH(TODAY + extradag),"99") + " tom " + SUBSTRING(STRING(ADD-INTERVAL(TODAY + extradag,11,"months"),"99999999"),1,6).
         END.
         ELSE IF ekalkkattemp.VINAMN = "Guru halvår" THEN DO:
            regmnr = MONTH(TODAY + extradag).
            RUN MANNAMN.P.
            ekalkkattemp.ANTAL = 6.
            ekalkkattemp.VINAMN = "Guru " + STRING(YEAR(TODAY + extradag)) + STRING(MONTH(TODAY + extradag),"99") + " tom " + SUBSTRING(STRING(ADD-INTERVAL(TODAY + extradag,5,"months"),"99999999"),1,6).
         END.   
         ELSE IF ekalkkattemp.VINAMN = "Guru kvartal" THEN DO:
            regmnr = MONTH(TODAY + extradag).
            
            IF AVAILABLE GuruGrundPrisTT THEN DO:
               ekalkkattemp.ANTAL = 3.
            END.
            ELSE ekalkkattemp.ANTAL = 1.
            RUN MANNAMN.P.
            ekalkkattemp.VINAMN = "Guru " + STRING(YEAR(TODAY + extradag)) + STRING(MONTH(TODAY + extradag),"99") + " tom " + SUBSTRING(STRING(ADD-INTERVAL(TODAY + extradag,2,"months"),"99999999"),1,6).
         END.
         ELSE RELEASE GuruGrundPrisTT NO-ERROR.
         IF AVAILABLE GuruGrundPrisTT THEN ekalkkattemp.PRIS =  GuruGrundPrisTT.PRIS.
         ekalkkattemp.TOTFAKT = ekalkkattemp.ANTAL * ekalkkattemp.PRIS.  
         RUN fritot_UI.
         antal_raknare = antal_raknare + 1.
         RUN setlastrowid_UI IN brwproc[6] (INPUT ROWID(ekalkkattemp)).
      END.    
      RUN openbdynspec_UI IN brwproc[6].        
      RUN lastselectdyn_UI IN brwproc[6].
      DISABLE RAD_VAL TOG_DIFF WITH FRAME {&FRAME-NAME}.
   END.
  
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPPANM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPPANM Dialog-Frame
ON CHOOSE OF BTN_UPPANM IN FRAME Dialog-Frame /* Ändra */
DO:
   RUN entryantal_UI.
   RUN andra_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEV Dialog-Frame
ON VALUE-CHANGED OF CMB_LEV IN FRAME Dialog-Frame /* Leverantörer */
DO:
   {muswait.i}                                 
   CMB_LEV = INPUT CMB_LEV.
   FIND FIRST levtemp WHERE levtemp.LEVNAMN = CMB_LEV 
   USE-INDEX LEV NO-LOCK NO-ERROR. 
   vald_lev = levtemp.LEVKOD.
   RUN setorgtitle_UI IN brwproc[2] (INPUT "Materiellista " + CMB_LEV).         
   RUN setcolsortvar_UI IN brwproc[2] (INPUT " WHERE LEVKOD = '" + STRING(levtemp.LEVKOD) + "' AND KALKNR = 0").
   RUN openbdynspec_UI IN brwproc[2].
/*    OPEN QUERY BRW_HLEV FOR EACH mtrltemp WHERE mtrltemp.LEVKOD = levtemp.LEVKOD AND */
/*    mtrltemp.KALKNR = 0 NO-LOCK.                                                     */
   {musarrow.i}     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_VAL Dialog-Frame
ON VALUE-CHANGED OF CMB_VAL IN FRAME Dialog-Frame /* Kategori */
DO:
   CMB_VAL = INPUT CMB_VAL.
   IF faktfriatemp.TYP BEGINS "PERS" OR faktfriatemp.TYP = "TRAKT" THEN DO: 
      faktfriatemp.FAKTTEXT = CMB_VAL.            
   END.   
   ELSE DO:
      FIND FIRST kalkkattemp WHERE kalkkattemp.TYP = faktfriatemp.TYP AND 
      kalkkattemp.VINAMN = CMB_VAL NO-LOCK.
      IF AVAILABLE kalkkattemp THEN DO:
         ASSIGN
         faktfriatemp.PRIS = kalkkattemp.PRIS
         faktfriatemp.OPRIS = kalkkattemp.OPRIS
         faktfriatemp.FAKTTEXT = kalkkattemp.VINAMN.       
      END.
   END.
   IF faktfriatemp.TYP BEGINS "PERS" THEN DO:
      DISPLAY CMB_VAL faktfriatemp.TOTKALK faktfriatemp.TOTALT
      WITH FRAME {&FRAME-NAME}.            
   END.
   ELSE IF faktfriatemp.TYP = "MASK" THEN DO:
      DISPLAY  
      faktfriatemp.FAKTTEXT   
      faktfriatemp.PRIS faktfriatemp.PRIS_ENHET faktfriatemp.ANTAL        
      faktfriatemp.TOTKALK faktfriatemp.TOTALT
      WITH FRAME {&FRAME-NAME}.
   END.
   ELSE IF faktfriatemp.TYP = "MIL" OR faktfriatemp.TYP = "TRAKT" THEN DO:
      DISPLAY 
      faktfriatemp.PRIS_ENHET faktfriatemp.ANTAL faktfriatemp.PRIS faktfriatemp.TOTKALK faktfriatemp.TOTALT
      WITH FRAME {&FRAME-NAME}.
   END.
   ELSE IF faktfriatemp.TYP = "OVER" THEN DO:
      DISPLAY faktfriatemp.PRIS_ENHET 
      faktfriatemp.ANTAL faktfriatemp.PRIS
      faktfriatemp.FAKTTEXT        
      faktfriatemp.TOTKALK faktfriatemp.TOTALT
      WITH FRAME {&FRAME-NAME}.
   END.
   ELSE IF faktfriatemp.TYP = "MATRL" THEN DO:
      DISPLAY faktfriatemp.PRIS_ENHET 
      faktfriatemp.ANTAL faktfriatemp.PRIS
      faktfriatemp.FAKTTEXT        
      faktfriatemp.TOTKALK faktfriatemp.TOTALT
      WITH FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME faktfriatemp.FAKTURERAD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktfriatemp.FAKTURERAD Dialog-Frame
ON MOUSE-SELECT-CLICK OF faktfriatemp.FAKTURERAD IN FRAME Dialog-Frame /* FAKTURERAD */
DO:
   faktfriatemp.FAKTURERAD = INPUT faktfriatemp.FAKTURERAD.
   IF faktfriatemp.FAKTURERAD = TRUE THEN ASSIGN faktfriatemp.FAKTURERAD = FALSE.
   ELSE ASSIGN faktfriatemp.FAKTURERAD = TRUE.
   DISPLAY faktfriatemp.FAKTURERAD WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_OK Dialog-Frame
ON CHOOSE OF FBTN_OK IN FRAME Dialog-Frame /* Ok */
DO: 
   RUN entryantal_UI.
   knappvar = 4.   
   RUN spara_UI.
   nya = FALSE.
   IF VALID-HANDLE(mtrlhmtapph) THEN DELETE PROCEDURE mtrlhmtapph.
   IF VALID-HANDLE(fakkoproch) THEN DELETE PROCEDURE fakkoproch. 
   IF VALID-HANDLE(laddaproch) THEN DELETE PROCEDURE laddaproch.
   IF VALID-HANDLE(tthandle) THEN DELETE OBJECT tthandle.
   IF VALID-HANDLE(anmarkapph) THEN DELETE PROCEDURE anmarkapph.
   APPLY "GO" TO FBTN_OK.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_OK Dialog-Frame
ON GO OF FBTN_OK IN FRAME Dialog-Frame /* Ok */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SPARA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SPARA Dialog-Frame
ON CHOOSE OF FBTN_SPARA IN FRAME Dialog-Frame /* Spara */
DO: 
   RUN entryantal_UI.
   knappvar = 4.   
   RUN spara_UI.
   EMPTY TEMP-TABLE faktfriatemp NO-ERROR. 
   frinr = frinr + 1.      
   CREATE faktfriatemp. 
   ASSIGN 
   faktfriatemp.ROWFRI = ?
   faktfriatemp.FAKTNR = fnr
   faktfriatemp.FDELNR = fdel
   faktfriatemp.FAKTURERAD = TRUE
   faktfriatemp.LOPNR = frinr
      
   faktfriatemp.TYP = vtyp.
   ENABLE RAD_VAL WITH FRAME {&FRAME-NAME}.        
   ASSIGN
   nya = TRUE.
   APPLY "VALUE-CHANGED" TO RAD_VAL.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SPARA Dialog-Frame
ON GO OF FBTN_SPARA IN FRAME Dialog-Frame /* Spara */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-BEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN Dialog-Frame
ON ANY-KEY OF FILL-IN-BEN IN FRAME Dialog-Frame /* Benämning */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-BEN Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-BEN IN FRAME Dialog-Frame /* Benämning */
DO:
   {muswait.i}
   {BENHMT2.I}     
   RUN initsok_UI (INPUT 1,INPUT aosok).
   {musarrow.i}                  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ENR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR Dialog-Frame
ON ANY-KEY OF FILL-IN-ENR IN FRAME Dialog-Frame /* Enr */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN-ENR IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ENR Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF FILL-IN-ENR IN FRAME Dialog-Frame /* Enr */
DO: 
   {muswait.i}
   {ENRHMT2.I}
   RUN initsok_UI (INPUT 2,INPUT posok).
   {musarrow.i}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME faktfriatemp.OANTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktfriatemp.OANTAL Dialog-Frame
ON ENTRY OF faktfriatemp.OANTAL IN FRAME Dialog-Frame /* OANTAL */
DO:
   IF faktfriatemp.TYP BEGINS "PERS" THEN DO:      
      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktfriatemp.OANTAL Dialog-Frame
ON LEAVE OF faktfriatemp.OANTAL IN FRAME Dialog-Frame /* OANTAL */
DO:
   RUN summa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME faktfriatemp.OPRIS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktfriatemp.OPRIS Dialog-Frame
ON ENTRY OF faktfriatemp.OPRIS IN FRAME Dialog-Frame /* PRIS/ENHET */
DO:
   IF faktfriatemp.TYP BEGINS "PERS" THEN DO:      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktfriatemp.OPRIS Dialog-Frame
ON LEAVE OF faktfriatemp.OPRIS IN FRAME Dialog-Frame /* PRIS/ENHET */
DO:
   RUN summa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME faktfriatemp.PRIS_ENHET
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktfriatemp.PRIS_ENHET Dialog-Frame
ON LEAVE OF faktfriatemp.PRIS_ENHET IN FRAME Dialog-Frame /* PRIS/ENHET */
DO:
   RUN summa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_SOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_SOK Dialog-Frame
ON VALUE-CHANGED OF RAD_SOK IN FRAME Dialog-Frame
DO:
  RAD_SOK = INPUT RAD_SOK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAL Dialog-Frame
ON VALUE-CHANGED OF RAD_VAL IN FRAME Dialog-Frame
DO:
   
   RAD_VAL = INPUT RAD_VAL.
   TOG_DIFF:HIDDEN = FALSE.
   BRW_VFRIP:HEIGHT =  BRW_MTRL:HEIGHT.
   FILL-IN-fritot:HIDDEN = TRUE.
   IF RAD_VAL = 1 THEN DO:
      faktfriatemp.TYP = "PERS".
      IF TOG_OVER = TRUE THEN faktfriatemp.TYP = "PERSO".
   END.
   ELSE IF RAD_VAL = 2 THEN DO:
      faktfriatemp.TYP = "MASK".      
   END.
   ELSE IF RAD_VAL = 3 THEN DO:
      faktfriatemp.TYP = "TRAKT".
   END.
   ELSE IF RAD_VAL = 4 THEN DO:
      faktfriatemp.TYP = "MIL".
   END.
   ELSE IF RAD_VAL = 5 THEN DO:
      faktfriatemp.TYP = "MATRL".
      faktfriatemp.ANTAL = 1.
   END.
   ELSE IF RAD_VAL = 6 THEN DO:
      faktfriatemp.TYP = "OVER".
   END.
   ELSE IF RAD_VAL = 7 THEN DO:
      FILL-IN-fritot:HIDDEN = FALSE.
      BRW_VFRIP:HEIGHT =  BRW_FRIP:HEIGHT.
      ASSIGN
      TOG_DIFF:HIDDEN = TRUE
      FILL-IN_MOMSEXTERNT:HIDDEN = TRUE 
      FILL-IN_MOMSKOD:HIDDEN = TRUE 
      FILL-IN_MOMSNR:HIDDEN = TRUE
      faktfriatemp.TYP = "FAKT".
      RUN fritot_UI.
   END.
   RUN visa_UI.
   APPLY "VALUE-CHANGED" TO CMB_LEV.
   DISPLAY RAD_VAL WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_DIFF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_DIFF Dialog-Frame
ON VALUE-CHANGED OF TOG_DIFF IN FRAME Dialog-Frame /* Differentierad moms */
DO:
   TOG_DIFF = INPUT TOG_DIFF.  
   IF TOG_DIFF = TRUE THEN DO:   
      RUN goma_UI.
      OPEN QUERY BRW_K4 FOR EACH Momstemp WHERE Momstemp.BORT = FALSE NO-LOCK BY Momstemp.MOMSNR BY Momstemp.MOMSKOD.
      ASSIGN            
      FILL-IN_MOMSEXTERNT:HIDDEN = FALSE 
      BRW_K4:HIDDEN = FALSE 
      FILL-IN_MOMSKOD:HIDDEN = FALSE 
      FILL-IN_MOMSNR:HIDDEN = FALSE.    
      IF faktfriatemp.MOMSID = 0  THEN DO:
         DISPLAY BRW_K4 WITH FRAME {&FRAME-NAME}.         
      END.   
      ELSE DO:
         FIND FIRST Momstemp WHERE Momstemp.MOMSID = faktfriatemp.MOMSID NO-LOCK NO-ERROR.          
         ASSIGN
         FILL-IN_MOMSEXTERNT = momstemp.MOMSEXTERNT
         FILL-IN_MOMSKOD     = momstemp.MOMSKOD    
         FILL-IN_MOMSNR      = momstemp.MOMSNR.
         DISPLAY FILL-IN_MOMSEXTERNT FILL-IN_MOMSKOD FILL-IN_MOMSNR WITH FRAME {&FRAME-NAME}.
         RUN setlastrowid_UI IN brwproc[4] (INPUT ROWID(momstemp)).              
         RUN lastselectdyn_UI IN brwproc[4].                
         
      END.
   END.
   ELSE DO:
      ASSIGN
      FILL-IN_MOMSEXTERNT:HIDDEN = TRUE 
      BRW_K4:HIDDEN = TRUE 
      FILL-IN_MOMSKOD:HIDDEN = TRUE 
      FILL-IN_MOMSNR:HIDDEN = TRUE.
      APPLY "VALUE-CHANGED" TO RAD_VAL.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_OFFERT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_OFFERT Dialog-Frame
ON VALUE-CHANGED OF TOG_OFFERT IN FRAME Dialog-Frame /* Offert */
DO:
   TOG_OFFERT = INPUT TOG_OFFERT.
   offert = TOG_OFFERT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_OVER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_OVER Dialog-Frame
ON VALUE-CHANGED OF TOG_OVER IN FRAME Dialog-Frame /* Övertid */
DO:
   TOG_OVER = INPUT TOG_OVER.   
   RUN visa_UI.
   RUN summa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME faktfriatemp.TOTALT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL faktfriatemp.TOTALT Dialog-Frame
ON LEAVE OF faktfriatemp.TOTALT IN FRAME Dialog-Frame /* TOTALT */
DO:
  /* offert = TRUE.   */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_ANM
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {DIA_M_START.I}
   {ALLSTARTDYN.I}     
   RUN hmtfastanm IN anmarkapph (INPUT "FAKT",OUTPUT TABLE fastanmtemp).
   FIND FIRST faktfriatemp NO-ERROR.
   frinr = faktfriatemp.LOPNR.
   IF faktfriatemp.TYP = "" THEN DO:      
      faktfriatemp.TYP = "PERS".
      ENABLE RAD_VAL WITH FRAME {&FRAME-NAME}.              
   END.   
   ELSE faktja = faktfriatemp.FAKTURERAD.
   IF faktfriatemp.TYP BEGINS "PERS" THEN RAD_VAL = 1.
   ELSE IF faktfriatemp.TYP = "MASK" THEN RAD_VAL = 2.
   ELSE IF faktfriatemp.TYP = "TRAKT" THEN RAD_VAL = 3.
   ELSE IF faktfriatemp.TYP = "MIL" THEN RAD_VAL = 4.
   ELSE IF faktfriatemp.TYP = "MATRL" THEN RAD_VAL = 5.
   ELSE IF faktfriatemp.TYP = "OVER" THEN RAD_VAL = 6.
   ELSE IF faktfriatemp.TYP BEGINS "FAKT" THEN RAD_VAL = 7.
   status-ok = TRUE.
   DO WHILE status-ok = TRUE:
      status-ok = CMB_AONR:DELETE(1) IN FRAME {&FRAME-NAME}.      
   END.  
   OPEN QUERY faktaonrq FOR EACH faktaonrtemp WHERE 
   faktaonrtemp.FAKTNR = faktfriatemp.FAKTNR NO-LOCK.   
   GET FIRST faktaonrq NO-LOCK.
   DO WHILE AVAILABLE(faktaonrtemp):
      status-ok = CMB_AONR:ADD-LAST(faktaonrtemp.AONR + " " + STRING(faktaonrtemp.DELNR,Guru.Konstanter:varforetypchar[1])).
      GET NEXT faktaonrq NO-LOCK.
   END.
   GET FIRST faktaonrq NO-LOCK.   
   IF AVAILABLE faktaonrtemp THEN DO:
      CMB_AONR:SCREEN-VALUE = faktaonrtemp.AONR + " " + STRING(faktaonrtemp.DELNR,Guru.Konstanter:varforetypchar[1]).
   END.   
   IF faktfriatemp.AONR NE "" THEN DO:
      ASSIGN CMB_AONR:SCREEN-VALUE = faktfriatemp.AONR + " " + STRING(faktfriatemp.DELNR,Guru.Konstanter:varforetypchar[1]).
   END.
   RUN startfriny_UI IN fakthmth 
   (INPUT infakplannr,INPUT fdelnrvar,INPUT faktfriatemp.LOPNR,
    OUTPUT TABLE beftemp,
    OUTPUT TABLE obeftemp,
    OUTPUT TABLE faktmtrltemp,
    OUTPUT TABLE kalkkattemp,
    OUTPUT TABLE GuruGrundPrisTT). 
   
   FIND FIRST kalkkattemp WHERE kalkkattemp.TYP BEGINS "FAKT" NO-LOCK.
   IF AVAILABLE kalkkattemp THEN DO:
      IF varfaktypnr = 3 OR varfaktypnr = 4 OR varfaktypnr = 7 OR varfaktypnr = 8 OR 
         varfaktypnr = 11 /*OR varfaktypnr = 12*/ 
      THEN musz = musz.
      ELSE status-ok = RAD_VAL:ADD-LAST("Fri prislista",7).
   END.
   RUN lev_UI.
   RUN visa_UI.           
   RUN enable_UI.          
   
   {FRMSIZED.I}   
   BRW_K4:HIDDEN = TRUE. 
   IF RAD_VAL = 7 THEN DO:
      FILL-IN-fritot:HIDDEN = FALSE.
   END.
   ELSE DO:
      FILL-IN-fritot:HIDDEN = TRUE.
      IF faktfriatemp.MOMSID NE 0 THEN DO:           
         TOG_DIFF = TRUE.      
         DISPLAY TOG_DIFF WITH FRAME {&FRAME-NAME}.
         APPLY "VALUE-CHANGED" TO TOG_DIFF.      
      END.
   END.
   
   IF nya = FALSE THEN DO:
      APPLY "ENTRY" TO faktfriatemp.ANTAL IN FRAME {&FRAME-NAME}.
   END.
   ASSIGN
   faktfriatemp.ANTAL:MODIFIED = FALSE 
   faktfriatemp.OANTAL:MODIFIED = FALSE 
   faktfriatemp.OPRIS:MODIFIED = FALSE 
   faktfriatemp.PRIS:MODIFIED = FALSE. 
   
   IF RAD_VAL = 5 THEN DO:
      ASSIGN
      FILL-IN-OVRK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
      BTN_AVB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
    /*  BTN_BORT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE*/.
   END.
   faktmtrltemp.ENR:LABEL IN BROWSE BRW_MTRL = Guru.Konstanter:genk.
   mtrltemp.ENR:LABEL IN BROWSE BRW_HLEV = Guru.Konstanter:genk. 
   FILL-IN-ENR:LABEL = Guru.Konstanter:genk.
   CMB_AONR:LABEL = Guru.Konstanter:gaok + " delnr".
   IF vartyp = 7 THEN TOG_DIFF:HIDDEN = TRUE.
   IF nya = FALSE THEN DO:
      FBTN_SPARA:HIDDEN IN FRAME {&FRAME-NAME} = TRUE. 
   END.
   IF Guru.Konstanter:globforetag = "elpa" THEN DO: 
      RAD_VAL = 7.
      DISPLAY RAD_VAL WITH FRAME {&FRAME-NAME}.
      
   END. 
   APPLY "VALUE-CHANGED" TO RAD_VAL.
   FRAME {&FRAME-NAME}:HIDDEN = FALSE. 
   
   IF RAD_VAL = 7 THEN DO:
      IF nya = FALSE THEN DO:
         GET FIRST BRW_VFRIP NO-LOCK.
         RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(ekalkkattemp)).              
         RUN lastselectdyn_UI IN brwproc[5].  
         APPLY "VALUE-CHANGED" TO BRW_VFRIP.   
         /*
         MESSAGE faktfriatemp.ANTAL VIEW-AS ALERT-BOX.
         RUN openbdynspec_UI IN brwproc[5].
         status-ok = BRW_VFRIP:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} .
         */
      END.
      ELSE DO:
         FIND FIRST kalkkattemp WHERE kalkkattemp.VINAMN = "Guru imånad" NO-LOCK NO-ERROR.
         IF AVAILABLE kalkkattemp THEN DO:
            RUN setlastrowid_UI IN brwproc[5] (INPUT ROWID(kalkkattemp)).              
            RUN lastselectdyn_UI IN brwproc[5].
         END.   
         
      END.   
   END.
   
   
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI Dialog-Frame 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   ASSIGN
   fastanmtemp.ANVANDARE:READ-ONLY IN BROWSE BRW_ANM = TRUE
   mtrltemp.ENR:READ-ONLY IN BROWSE BRW_HLEV = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_ANM:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_HLEV:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[3]
      (INPUT BRW_MTRL:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[4]
      (INPUT BRW_K4:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[5]
      (INPUT BRW_FRIP:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[6]
      (INPUT BRW_VFRIP:HANDLE IN FRAME {&FRAME-NAME}).
   RUN settitlenum_UI IN brwproc[2] (INPUT TRUE).
   RUN settitlenum_UI IN brwproc[3] (INPUT TRUE).
   RUN setcolsortvar_UI IN brwproc[1] (INPUT "fastanmtemp.PROGRAM = 'FAKT'").
   RUN setcolsortvar_UI IN brwproc[5] (INPUT "kalkkattemp.TYP BEGINS 'FAKT'").
   RUN setcolindex_UI IN brwproc[5] (INPUT "NAMN BY VINAMN").
   RUN setcolindex_UI IN brwproc[6] (INPUT "NAMN BY VINAMN").
   IF Guru.Konstanter:appcon THEN DO:
      RUN ANMARKAPP.P PERSISTENT SET anmarkapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN ANMARKAPP.P PERSISTENT SET anmarkapph.
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN MTRLHMT.P PERSISTENT SET mtrlhmtapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MTRLHMT.P PERSISTENT SET mtrlhmtapph.
   END.  
   tthandle = TEMP-TABLE huvlevtemp:HANDLE.
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "HUVUDLEV", INPUT "").
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "HUVUDLEV", INPUT "").
   END.
   /*GGG 050215*/
   FIND FIRST huvlevtemp WHERE huvlevtemp.DEP-NR = 999 NO-LOCK NO-ERROR.
   IF AVAILABLE huvlevtemp THEN DO:
      vald_kundlev = huvlevtemp.LEVKOD.
   END.
   ELSE DO:
      FIND FIRST levtemp WHERE levtemp.LEVKOD NE "0"
      AND levtemp.BORTTAG = FALSE NO-LOCK NO-ERROR.
      vald_kundlev = levtemp.LEVKOD.
   END.
   vald_lev = vald_kundlev.
   /*GGG*/
   tthandle = TEMP-TABLE momstemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "MOMSTAB", INPUT " ").
   RUN addfillin_UI IN brwproc[1] 
      (INPUT FILL-IN_SFORNAMN:HANDLE IN FRAME {&FRAME-NAME}, INPUT "ANMARK").
   RUN addfillin_UI IN brwproc[1] 
      (INPUT FILL-IN_SPERSONALKOD:HANDLE IN FRAME {&FRAME-NAME}, INPUT "ANVANDARE"). 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra_UI Dialog-Frame 
PROCEDURE andra_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   status-ok = BRW_ANM:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF status-ok = FALSE THEN RETURN.
  
   IF fastanmtemp.ANVANDARE = Guru.Konstanter:globanv THEN DO:
      {muswait.i}
      ASSIGN
      brow = fastanmtemp.FASTANVROW
      anmarvar = fastanmtemp.ANMARK.
      RUN ANDAMARKA.W (INPUT anmarvar,INPUT fastanmtemp.FASTANVROW).
      RUN hmtfastanm IN anmarkapph (INPUT "FAKT",OUTPUT TABLE fastanmtemp).
      RUN openbdynspec_UI IN brwproc[1].
      FIND FIRST fastanmtemp WHERE fastanmtemp.FASTANVROW = brow NO-LOCK NO-ERROR.
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(fastanmtemp)).              
      RUN lastselectdyn_UI IN brwproc[1].           
      {musarrow.i}       
   END.
   ELSE DO:
      MESSAGE "Du kan inte ändra andras sparade texter!"
      VIEW-AS ALERT-BOX.            
   END.   
   musz = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE anm_UI Dialog-Frame 
PROCEDURE anm_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   status-ok = BRW_ANM:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF status-ok = FALSE THEN RETURN.
   faktfriatemp.FAKTTEXT = fastanmtemp.ANMARK.   
   DISPLAY faktfriatemp.FAKTTEXT WITH FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bortanm_UI Dialog-Frame 
PROCEDURE bortanm_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   status-ok = BRW_ANM:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF status-ok = FALSE THEN RETURN.
   IF fastanmtemp.ANVANDARE = Guru.Konstanter:globanv THEN DO:
      MESSAGE "Vill du verkligen ta bort denna fasta text?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE fastanmtemp.ANMARK
      UPDATE answer AS LOGICAL.
      IF answer THEN DO TRANSACTION:
         {muswait.i}
         RUN bortanm IN anmarkapph (INPUT fastanmtemp.FASTANVROW). 
         DELETE fastanmtemp. 
         RUN selnextprevrow_UI IN brwproc[1].
         RUN refreshbrw_UI IN brwproc[1].
         RUN lastselectdyn_UI IN brwproc[1].        
         IF NOT AVAILABLE fastanmtemp THEN DO:
            ASSIGN        
            BTN_BORTANM:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
            BTN_UPPANM:HIDDEN IN FRAME {&FRAME-NAME} = TRUE            
            BTN_KONTO:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
            FILL-IN_SFORNAMN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
            FILL-IN_SPERSONALKOD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
         END.
         
      END.
      {musarrow.i}       
   END.
   ELSE DO:
      MESSAGE "Du kan inte ta bort andras sparade texter!"
      VIEW-AS ALERT-BOX.      
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bort_UI Dialog-Frame 
PROCEDURE bort_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   MESSAGE "Vill du ta bort denna post ?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val1 AS LOGICAL.
   CASE val1:
      WHEN TRUE THEN DO:
         ASSIGN
         knappvar = 2.          
      END.
      WHEN FALSE THEN DO:
         knappvar = 3.                   
      END.
   END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY RAD_VAL TOG_OFFERT CMB_AONR FILL-IN-fritot TOG_DIFF RAD_SOK 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE faktfriatemp THEN 
    DISPLAY faktfriatemp.TOTKALK faktfriatemp.TOTALT faktfriatemp.FAKTURERAD 
      WITH FRAME Dialog-Frame.
  ENABLE TOG_OFFERT faktfriatemp.TOTALT FBTN_SPARA faktfriatemp.FAKTURERAD 
         CMB_AONR TOG_DIFF BRW_HLEV BRW_VFRIP BRW_FRIP BRW_K4 BRW_ANM FBTN_OK 
         RAD_SOK BTN_AVB 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE entryantal_UI Dialog-Frame 
PROCEDURE entryantal_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   IF entrymtrlantal = TRUE THEN DO:
      APPLY "LEAVE" TO ekalkkattemp.ANTAL IN BROWSE BRW_VFRIP.      
      /*
      IF AVAILABLE fastkalktemp THEN DO:
         DISPLAY fastkalktemp.ANTAL WITH BROWSE BRW_KALK NO-ERROR.
         fastkalktemp.ANTAL = INPUT BROWSE BRW_KALK fastkalktemp.ANTAL.
         DISPLAY fastkalktemp.ANTAL WITH BROWSE BRW_KALK NO-ERROR.
      END.
      */
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fritotminus_UI Dialog-Frame 
PROCEDURE fritotminus_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fritot_UI Dialog-Frame 
PROCEDURE fritot_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FILL-IN-fritot = 0.
   FOR EACH ekalkkattempbuff:
      FILL-IN-fritot = FILL-IN-fritot + ekalkkattempbuff.TOTFAKT.
      
   END.
   DISPLAY FILL-IN-fritot WITH FRAME Dialog-Frame.     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE goma_UI Dialog-Frame 
PROCEDURE goma_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN        

   BRW_HLEV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   BRW_K4:HIDDEN = TRUE 
   FILL-IN_MOMSEXTERNT:HIDDEN = TRUE 
   FILL-IN_MOMSKOD:HIDDEN = TRUE 
   FILL-IN_MOMSNR:HIDDEN = TRUE
   BRW_MTRL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   btn_back:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   btn_over:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
/*    FILL-IN-SOK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE */
   FILL-IN-BEN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   FILL-IN-ENR:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   RAD_SOK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   CMB_LEV:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FILL-IN-OVRK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   RECT-21:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BTN_BORTANM:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BTN_UPPANM:HIDDEN IN FRAME {&FRAME-NAME} = TRUE         
   BTN_KONTO:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FILL-IN_SFORNAMN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   FILL-IN_SPERSONALKOD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BRW_ANM:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
   BTN_NYANM:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BRW_FRIP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   BRW_VFRIP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initsok_UI Dialog-Frame 
PROCEDURE initsok_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/           
   DEFINE INPUT  PARAMETER vad AS INTEGER    NO-UNDO.
   DEFINE INPUT PARAMETER sokpa AS CHARACTER NO-UNDO.
   DEFINE VARIABLE orgfraga AS CHARACTER NO-UNDO.
   IF vad = 1 THEN DO:
      orgfraga = " WHERE KALKNR = " + STRING(0) + " AND LEVKOD = '" + STRING(vald_lev) + "' USE-INDEX LEV".
      tth = TEMP-TABLE mtrltemp:HANDLE.
      EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
      CREATE valsoktemp.
      ASSIGN 
      valsoktemp.SOKCHAR[1] = "MTRL"     /*Skarp tabell*/
      valsoktemp.SOKCHAR[2] = orgfraga   /*Öppningsquery*/
      valsoktemp.SOKCHAR[3] = "BENAMNING" /*sökfält*/
      valsoktemp.SOKCHAR[4] = "MTRLROW"  /*temptabells faltnamn för rowid*/
      valsoktemp.SOKCHAR[5] = sokpa.      /*sök på*/       
      RUN sokhmt_UI IN  brwproc[2] (INPUT TABLE valsoktemp).  
   END.
   IF vad = 2 THEN DO:
     orgfraga = " WHERE KALKNR = " + STRING(0) + " AND LEVKOD = '" + STRING(vald_lev) + "' USE-INDEX LEV".
     tth = TEMP-TABLE mtrltemp:HANDLE.
     EMPTY TEMP-TABLE valsoktemp NO-ERROR. 
     CREATE valsoktemp.
     ASSIGN 
     valsoktemp.SOKCHAR[1] = "MTRL"     /*Skarp tabell*/
     valsoktemp.SOKCHAR[2] = orgfraga   /*Öppningsquery*/
     valsoktemp.SOKCHAR[3] = "ENR" /*sökfält*/
     valsoktemp.SOKCHAR[4] = "MTRLROW"  /*temptabells faltnamn för rowid*/
     valsoktemp.SOKCHAR[5] = sokpa.      /*sök på*/       
     RUN sokhmt_UI IN  brwproc[2] (INPUT TABLE valsoktemp).  
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lev_UI Dialog-Frame 
PROCEDURE lev_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   tthandle = TEMP-TABLE levtemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "LEVERANTOR", INPUT " ").
   status-ok = CMB_LEV:DELETE("0") IN FRAME {&FRAME-NAME}. 
   FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_kundlev
   USE-INDEX LEV NO-LOCK NO-ERROR.
/*    FIND FIRST levtemp USE-INDEX LEV NO-LOCK NO-ERROR. */
   ASSIGN
   status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN) IN FRAME {&FRAME-NAME}.
   CMB_LEV:SCREEN-VALUE = levtemp.LEVNAMN.
   
   FOR EACH levtemp WHERE levtemp.LEVKOD NE vald_kundlev AND 
   levtemp.LEVKOD NE "0" AND levtemp.BORTTAG = FALSE USE-INDEX LEV NO-LOCK:      
      status-ok = CMB_LEV:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.              
   END. 
                
   CMB_LEV = INPUT CMB_LEV.
   ASSIGN 
   vald_lev = vald_kundlev.       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nyfaktmtrl_UI Dialog-Frame 
PROCEDURE nyfaktmtrl_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO TRANSACTION:
     CREATE faktmtrltemp.
     ASSIGN 
     faktmtrltemp.FAKTNR = faktfriatemp.FAKTNR
     faktmtrltemp.FDELNR = faktfriatemp.FDELNR
     faktmtrltemp.LOPNR = faktfriatemp.LOPNR
     faktmtrltemp.Benamning  = mtrltemp.Benamning         
     faktmtrltemp.BERKVANT = mtrltemp.BERKVANT
     faktmtrltemp.Enhet = mtrltemp.Enhet
     faktmtrltemp.Enr = mtrltemp.Enr   
     faktmtrltemp.NPRIS = mtrltemp.NPRIS.
     faktfriatemp.PRIS_ENHET = faktfriatemp.PRIS_ENHET + faktmtrltemp.NPRIS * faktmtrltemp.BERKVANT.   
     DISPLAY faktfriatemp.PRIS_ENHET WITH FRAME {&FRAME-NAME}.
     RUN summa_UI.
  END.   
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ny_UI Dialog-Frame 
PROCEDURE ny_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    {muswait.i}  
   RUN nyanm IN anmarkapph (INPUT "",INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:globomr,INPUT "FAKT",OUTPUT brow,
                            OUTPUT TABLE fastanmtemp APPEND).
   FIND FIRST fastanmtemp WHERE fastanmtemp.FASTANVROW = brow NO-LOCK NO-ERROR. 
   faktfriatemp.FAKTTEXT = INPUT FRAME {&FRAME-NAME} faktfriatemp.FAKTTEXT.  
   RUN ANDAMARKA.W (INPUT fastanmtemp.ANMARK, INPUT fastanmtemp.FASTANVROW).
   {musarrow.i}
   IF musz = FALSE THEN DO:   
      ASSIGN
      FILL-IN-OVRK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.      
      ENABLE BRW_ANM BTN_NYANM BTN_UPPANM BTN_BORTANM BTN_KONTO FILL-IN_SFORNAMN FILL-IN_SPERSONALKOD
      WITH FRAME {&FRAME-NAME}.
      RUN hmtfastanm IN anmarkapph (INPUT "FAKT",OUTPUT TABLE fastanmtemp).
      RUN openbdynspec_UI IN brwproc[1].
      FIND FIRST fastanmtemp WHERE fastanmtemp.FASTANVROW = brow NO-LOCK NO-ERROR.
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(fastanmtemp)).              
      RUN lastselectdyn_UI IN brwproc[1].                
   END.    
   ELSE DO:
      
      RUN bortanm IN anmarkapph (INPUT brow). 
      DELETE fastanmtemp. 
      
   END.
   musz = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sokben_UI Dialog-Frame 
PROCEDURE sokben_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/  
   felmedd = "".
   begvar = ?.
   RUN benhmt_UI IN mtrlhmtapph (INPUT FILL-IN-BEN,INPUT FALSE,INPUT begvar,INPUT levtemp.LEVKOD,
                                 OUTPUT felmedd,OUTPUT TABLE mtrltemp,OUTPUT TABLE satstemp).  
   IF felmedd NE "" THEN DO:
      MESSAGE felmedd VIEW-AS ALERT-BOX TITLE "Sökning".         
      felmedd = "".
      RUN openbdyn_UI IN brwproc[2] (INPUT "").
      APPLY "ENTRY" TO FILL-IN-BEN IN FRAME {&FRAME-NAME}.      
   END.   
   RUN openbdyn_UI IN brwproc[2] (INPUT "").
   FIND FIRST mtrltemp NO-LOCK NO-ERROR.
   IF AVAILABLE mtrltemp THEN DO:
      RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(mtrltemp)).
      RUN lastselectdyn_UI IN brwproc[2].
   END.      
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE spara_UI Dialog-Frame 
PROCEDURE spara_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/       
   CMB_AONR = INPUT FRAME {&FRAME-NAME} CMB_AONR.
   IF RAD_VAL = 1 THEN DO:
      ASSIGN      
      CMB_VAL = INPUT FRAME {&FRAME-NAME} CMB_VAL        
      faktfriatemp.OANTAL = INPUT faktfriatemp.OANTAL
      faktfriatemp.OPRIS = INPUT faktfriatemp.OPRIS
      faktfriatemp.PRIS_ENHET = INPUT faktfriatemp.PRIS_ENHET
      faktfriatemp.ANTAL = INPUT faktfriatemp.ANTAL    
      faktfriatemp.TOTALT = INPUT faktfriatemp.TOTALT.
      ASSIGN      
      faktfriatemp.FAKTTEXT = CMB_VAL.     
      IF TOG_OVER = TRUE THEN DO:
         FIND FIRST obeftemp WHERE obeftemp.OTEXT = faktfriatemp.FAKTTEXT 
         NO-LOCK NO-ERROR.
         SUBSTRING(faktfriatemp.FAKTTEXT,100) = obeftemp.BEFATTNING.          
         faktfriatemp.TYP = "PERSO".
      END.
      ELSE DO:
         FIND FIRST beftemp WHERE beftemp.NAMN = faktfriatemp.FAKTTEXT 
         NO-LOCK NO-ERROR.
         SUBSTRING(faktfriatemp.FAKTTEXT,100) = beftemp.BEFATTNING.
      END.
   END.
   ELSE IF RAD_VAL = 2 THEN DO: 
      ASSIGN      
      faktfriatemp.FAKTTEXT = INPUT faktfriatemp.FAKTTEXT
      faktfriatemp.OANTAL = INPUT faktfriatemp.OANTAL
      faktfriatemp.OPRIS = INPUT faktfriatemp.OPRIS
      faktfriatemp.PRIS_ENHET = INPUT faktfriatemp.PRIS_ENHET
      faktfriatemp.ANTAL = INPUT faktfriatemp.ANTAL
      faktfriatemp.TOTALT = INPUT faktfriatemp.TOTALT.          
   END.
   ELSE IF RAD_VAL = 3 OR RAD_VAL = 4 THEN DO:
      ASSIGN     
      CMB_VAL = INPUT CMB_VAL        
      faktfriatemp.PRIS_ENHET = INPUT faktfriatemp.PRIS_ENHET
      faktfriatemp.ANTAL = INPUT faktfriatemp.ANTAL
      faktfriatemp.TOTALT = INPUT faktfriatemp.TOTALT. 
      ASSIGN      
      faktfriatemp.FAKTTEXT = CMB_VAL.
   END.
   ELSE IF RAD_VAL = 5 OR RAD_VAL = 6 THEN DO: 
      ASSIGN
      faktfriatemp.FAKTTEXT = INPUT faktfriatemp.FAKTTEXT
      faktfriatemp.PRIS_ENHET = INPUT faktfriatemp.PRIS_ENHET
      faktfriatemp.ANTAL = INPUT faktfriatemp.ANTAL
      faktfriatemp.TOTALT = INPUT faktfriatemp.TOTALT.      
   END.
   IF RAD_VAL < 7 THEN DO:
      ASSIGN
      faktfriatemp.AONR = SUBSTRING(CMB_AONR,1,(INDEX(CMB_AONR," ") - 1))
      faktfriatemp.DELNR = INTEGER(SUBSTRING(CMB_AONR,(INDEX(CMB_AONR," ") + 1))). 
      IF AVAILABLE Momstemp THEN DO:
         ASSIGN
         faktfriatemp.MOMSEXTERNT = Momstemp.MOMSEXTERNT
         faktfriatemp.MOMSID = Momstemp.MOMSID.    
      END.  
      ASSIGN       
      fnr = faktfriatemp.FAKTNR
      fdel = faktfriatemp.FDELNR.
      vtyp = faktfriatemp.TYP.        
      RUN summa_UI.
      IF faktfriatemp.FAKTURERAD = TRUE THEN DO:
         FIND FIRST gfaktemp WHERE gfaktemp.ORDNING = 3 NO-ERROR.
         ASSIGN                                     
         gfaktemp.TOTALT = gfaktemp.TOTALT + faktfriatemp.TOTALT.
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
         IF faktfriatemp.TYP = "OVER" THEN DO:
            gfaktemp.OVRIG = gfaktemp.OVRIG + faktfriatemp.TOTALT.
         END.
         IF faktfriatemp.TYP = "MATRL" THEN DO:
            gfaktemp.MTRL = gfaktemp.MTRL + faktfriatemp.TOTALT.
         END.
        
         IF rundavar = TRUE THEN DO:                         
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
         END.
      END.
      IF kreditvar = FALSE THEN DO:  
         RUN sparfria_UI IN fakthmth (OUTPUT frirow,INPUT TABLE faktfriatemp).
         IF faktfriatemp.TYP = "MATRL" THEN DO:
            RUN sparmtrl_UI IN fakthmth 
            (INPUT infakplannr,INPUT fdelnrvar,INPUT faktfriatemp.LOPNR,INPUT TABLE faktmtrltemp).
         END.
      END.
      ELSE DO:
         RUN sparfriak_UI IN fakthmth (OUTPUT frirow,INPUT TABLE faktfriatemp).
         IF faktfriatemp.TYP = "MATRL" THEN DO:
            RUN sparmtrlk_UI IN fakthmth 
            (INPUT infakplannr,INPUT fdelnrvar,INPUT faktfriatemp.LOPNR,INPUT TABLE faktmtrltemp).
         END.
      END.
      EMPTY TEMP-TABLE faktmtrltemp NO-ERROR.   
   END.
   IF RAD_VAL = 7 THEN DO:
      ASSIGN       
      fnr = faktfriatemp.FAKTNR
      fdel = faktfriatemp.FDELNR.
      vtyp = faktfriatemp.TYP.        
      IF faktfriatemp.ROWFRIA = ? THEN DELETE faktfriatemp.
      FOR EACH ekalkkattemp:
         IF ekalkkattemp.ROWFRIA NE ? THEN DO:
            FIND FIRST faktfriatemp WHERE faktfriatemp.ROWFRIA = ekalkkattemp.ROWFRIA NO-LOCK NO-ERROR.
            IF NOT AVAILABLE faktfriatemp THEN DO:
               frinr = frinr + 1.      
               CREATE faktfriatemp.
               ASSIGN
               faktfriatemp.FAKTURERAD = TRUE
               faktfriatemp.LOPNR = frinr.
            END.
         END.
         ELSE DO:
            frinr = frinr + 1.      
            CREATE faktfriatemp.
            ASSIGN
            faktfriatemp.FAKTURERAD = TRUE
            faktfriatemp.LOPNR = frinr.
         END.
         ASSIGN 
         faktfriatemp.ROWFRI = ekalkkattemp.ROWFRIA
         faktfriatemp.ANTAL = ekalkkattemp.ANTAL
         faktfriatemp.ENHET = "st"
         faktfriatemp.FAKTTEXT = ekalkkattemp.VINAMN
         faktfriatemp.PRIS_ENHET = ekalkkattemp.PRIS
         faktfriatemp.TOTALT =  ekalkkattemp.PRIS * ekalkkattemp.ANTAL
         faktfriatemp.FAKTNR = fnr
         faktfriatemp.FDELNR = fdel              
         faktfriatemp.TOTKALK = ekalkkattemp.PRIS * ekalkkattemp.ANTAL
         faktfriatemp.TYP = "FAKT" + STRING(ekalkkattemp.RADNR).
         ASSIGN
         faktfriatemp.AONR = SUBSTRING(CMB_AONR,1,(INDEX(CMB_AONR," ") - 1))
         faktfriatemp.DELNR = INTEGER(SUBSTRING(CMB_AONR,(INDEX(CMB_AONR," ") + 1))). 
         /*
         IF AVAILABLE Momstemp THEN DO:
            ASSIGN
            faktfriatemp.MOMSEXTERNT = Momstemp.MOMSEXTERNT
            faktfriatemp.MOMSID = Momstemp.MOMSID.    
         END.  
         */
         IF faktfriatemp.FAKTURERAD = TRUE THEN DO:
            FIND FIRST gfaktemp WHERE gfaktemp.ORDNING = 3 NO-ERROR.
            ASSIGN            
            gfaktemp.TOTALT = gfaktemp.TOTALT + faktfriatemp.TOTALT.
            IF faktfriatemp.TYP BEGINS "FAKT" THEN DO:
               gfaktemp.OVRIG = gfaktemp.OVRIG + faktfriatemp.TOTALT.
            END.
            IF rundavar = TRUE THEN DO:                         
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
            END.
         END.
      END.
      IF kreditvar = FALSE THEN DO:  
         RUN sparfria_UI IN fakthmth (OUTPUT frirow,INPUT TABLE faktfriatemp).
      END.
      ELSE DO:
         RUN sparfriak_UI IN fakthmth (OUTPUT frirow,INPUT TABLE faktfriatemp).
      END.
      EMPTY TEMP-TABLE ekalkkattemp NO-ERROR. 
      EMPTY TEMP-TABLE faktmtrltemp NO-ERROR.   
      RUN refreshbrw_UI IN brwproc[6].
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE summa_UI Dialog-Frame 
PROCEDURE summa_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN
   faktfriatemp.ANTAL = INPUT FRAME {&FRAME-NAME} faktfriatemp.ANTAL
   faktfriatemp.OANTAL = INPUT faktfriatemp.OANTAL
   faktfriatemp.OPRIS = INPUT faktfriatemp.OPRIS
   faktfriatemp.PRIS_ENHET = INPUT faktfriatemp.PRIS_ENHET.
   faktfriatemp.TOTKALK = faktfriatemp.ANTAL * faktfriatemp.PRIS_ENHET + faktfriatemp.OANTAL * faktfriatemp.OPRIS.   
   IF offert = TRUE THEN DO:
      DISABLE RAD_VAL WITH FRAME {&FRAME-NAME}. 
      faktfriatemp.TOTALT = INPUT faktfriatemp.TOTALT.
   END.
   ELSE DO:
      IF faktfriatemp.ANTAL:MODIFIED = TRUE OR
      faktfriatemp.OANTAL:MODIFIED = TRUE OR
      faktfriatemp.OPRIS:MODIFIED = TRUE OR
      faktfriatemp.PRIS:MODIFIED = TRUE 
      THEN DO:
         DISABLE RAD_VAL WITH FRAME {&FRAME-NAME}.
         faktfriatemp.TOTALT = faktfriatemp.TOTKALK.
      END.
      
   END.   
   DISPLAY faktfriatemp.PRIS_ENHET faktfriatemp.TOTKALK faktfriatemp.TOTALT WITH FRAME {&FRAME-NAME}.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visa_UI Dialog-Frame 
PROCEDURE visa_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN goma_UI.
   IF RAD_VAL = 7 THEN DO:
      ASSIGN
      TOG_OFFERT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      faktfriatemp.PRIS_ENHET:HIDDEN = TRUE
      faktfriatemp.ANTAL:HIDDEN = TRUE
      faktfriatemp.TOTKALK:HIDDEN = TRUE
      faktfriatemp.TOTALT:HIDDEN = TRUE.     
   END.
   IF TOG_DIFF = TRUE THEN DO:
      DISPLAY FILL-IN_MOMSEXTERNT FILL-IN_MOMSKOD FILL-IN_MOMSNR WITH FRAME {&FRAME-NAME}.
   END.
   status-ok = TRUE.
   DO WHILE status-ok = TRUE:
      status-ok = CMB_VAL:DELETE(1) IN FRAME {&FRAME-NAME}.      
   END.
   /*HÄR FAKTIMMAR*/
   IF RAD_VAL = 1 THEN DO:                 
      IF faktfriatemp.TYP = "PERS" AND TOG_OVER = FALSE THEN DO:                 
         OPEN QUERY beftq FOR EACH beftemp NO-LOCK.
         GET FIRST beftq NO-LOCK.   
         IF AVAILABLE beftemp AND nya = TRUE THEN DO:
            ASSIGN
            faktfriatemp.PRIS = 0            
            faktfriatemp.FAKTTEXT = beftemp.NAMN.
         END.
         DO WHILE AVAILABLE(beftemp):
            status-ok = CMB_VAL:ADD-LAST(beftemp.NAMN).
            GET NEXT beftq NO-LOCK.
         END.
         FIND FIRST beftemp WHERE beftemp.NAMN = CMB_VAL NO-ERROR.
         IF NOT AVAILABLE beftemp THEN DO:
            FIND FIRST obeftemp WHERE obeftemp.OTEXT = CMB_VAL NO-ERROR.
            FIND FIRST beftemp WHERE beftemp.BEFATTNING = obeftemp.BEFATTNING NO-ERROR.
            IF AVAILABLE beftemp THEN CMB_VAL = beftemp.NAMN.
            ELSE DO:
               FIND FIRST beftemp NO-ERROR.
               IF AVAILABLE beftemp THEN CMB_VAL = beftemp.NAMN.
            END.
         END.
      END.   
      ELSE DO:
         TOG_OVER = TRUE.
         DISPLAY TOG_OVER WITH FRAME {&FRAME-NAME}. 
         OPEN QUERY obeftq FOR EACH obeftemp NO-LOCK.
         GET FIRST obeftq NO-LOCK.   
         IF AVAILABLE obeftemp AND nya = TRUE THEN DO:
            ASSIGN
            faktfriatemp.PRIS = 0     
            faktfriatemp.FAKTTEXT = obeftemp.OTEXT.
         END.
         DO WHILE AVAILABLE(obeftemp):
            status-ok = CMB_VAL:ADD-LAST(obeftemp.OTEXT).
            GET NEXT obeftq NO-LOCK.
         END.
         FIND FIRST obeftemp WHERE obeftemp.OTEXT = CMB_VAL NO-ERROR.         
         IF NOT AVAILABLE obeftemp THEN DO:
            FIND FIRST beftemp WHERE beftemp.NAMN = CMB_VAL NO-ERROR.
            FIND FIRST obeftemp WHERE obeftemp.BEFATTNING = beftemp.BEFATTNING NO-ERROR.
            IF AVAILABLE obeftemp THEN CMB_VAL = obeftemp.OTEXT.
            ELSE DO:
               FIND FIRST obeftemp NO-ERROR.
               IF AVAILABLE obeftemp THEN CMB_VAL = obeftemp.OTEXT.
            END.
         END.
      END.
   END.
   ELSE IF RAD_VAL = 3 THEN DO:                 
      status-ok = CMB_VAL:ADD-LAST("Endagsförrättning"). 
      status-ok = CMB_VAL:ADD-LAST("Flerdygnsförrättning").
      status-ok = CMB_VAL:ADD-LAST("Utlansförrättning").
   END.
   ELSE IF RAD_VAL < 7 THEN DO:
      OPEN QUERY kgoriq FOR EACH kalkkattemp WHERE 
      kalkkattemp.TYP = faktfriatemp.TYP NO-LOCK.
      GET FIRST kgoriq NO-LOCK.   
      IF AVAILABLE kalkkattemp AND nya = TRUE THEN DO:
         ASSIGN
         faktfriatemp.PRIS = kalkkattemp.PRIS
         faktfriatemp.OPRIS = kalkkattemp.OPRIS     
         faktfriatemp.FAKTTEXT = kalkkattemp.VINAMN.
      END.
      DO WHILE AVAILABLE(kalkkattemp):
         status-ok = CMB_VAL:ADD-LAST(kalkkattemp.VINAMN).
         GET NEXT kgoriq NO-LOCK.
      END.
      
   END.  
   
   ASSIGN
   BTN_AVB:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
   CMB_VAL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE   
   TOG_OVER:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   faktfriatemp.FAKTTEXT:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   faktfriatemp.OANTAL:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
   faktfriatemp.OPRIS:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   IF RAD_VAL = 1 THEN DO:
      CMB_VAL:SCREEN-VALUE = SUBSTRING(faktfriatemp.FAKTTEXT,1,99).
   END.
   IF RAD_VAL = 3 THEN DO:
      IF nya = TRUE THEN ASSIGN CMB_VAL:SCREEN-VALUE = "Endagsförrättning" NO-ERROR.    
      ELSE ASSIGN CMB_VAL:SCREEN-VALUE = faktfriatemp.FAKTTEXT NO-ERROR.     
   END.
   IF RAD_VAL = 4 THEN ASSIGN CMB_VAL:SCREEN-VALUE = faktfriatemp.FAKTTEXT NO-ERROR.    
   IF RAD_VAL = 1 THEN DO:     
      ASSIGN     
      faktfriatemp.ENHET = "h"
      faktfriatemp.ANTAL:LABEL = "Timmar"
      faktfriatemp.OANTAL:LABEL = "Övertidstimmar".
      ENABLE CMB_VAL TOG_OVER 
      faktfriatemp.PRIS_ENHET 
      faktfriatemp.ANTAL 
      WITH FRAME {&FRAME-NAME}.
      DISPLAY CMB_VAL TOG_OVER 
      faktfriatemp.PRIS_ENHET faktfriatemp.ANTAL 
      faktfriatemp.TOTKALK faktfriatemp.TOTALT
      WITH FRAME {&FRAME-NAME}.
   END.
   ELSE IF RAD_VAL = 2 THEN DO:   
      ASSIGN      
      faktfriatemp.ENHET = "h"
      faktfriatemp.ANTAL:LABEL = "Timmar"
      faktfriatemp.OANTAL:LABEL = "Övertidstimmar".
      ENABLE 
      faktfriatemp.FAKTTEXT   
      faktfriatemp.PRIS_ENHET 
      faktfriatemp.ANTAL
      WITH FRAME {&FRAME-NAME}.
      DISPLAY  
      faktfriatemp.FAKTTEXT   
      faktfriatemp.PRIS_ENHET faktfriatemp.ANTAL        
      faktfriatemp.TOTKALK faktfriatemp.TOTALT
      WITH FRAME {&FRAME-NAME}.
   END.
   ELSE IF RAD_VAL = 3 OR RAD_VAL = 4 THEN DO:
      ASSIGN      
      faktfriatemp.ENHET = "st"
      faktfriatemp.ANTAL:LABEL = "Antal".
      ENABLE CMB_VAL 
      faktfriatemp.PRIS_ENHET faktfriatemp.ANTAL 
      WITH FRAME {&FRAME-NAME}.
      DISPLAY 
      faktfriatemp.PRIS_ENHET faktfriatemp.ANTAL faktfriatemp.TOTKALK faktfriatemp.TOTALT
      WITH FRAME {&FRAME-NAME}.
   END.
   ELSE IF RAD_VAL = 6 THEN DO:      
     /* CMB_VAL = faktfriatemp.FAKTTEXT.
      ASSIGN CMB_VAL:SCREEN-VALUE = faktfriatemp.FAKTTEXT NO-ERROR.    */
      ASSIGN
      faktfriatemp.ENHET = "st"
      faktfriatemp.ANTAL:LABEL = "Antal".
      ENABLE  
  /*       CMB_VAL*/
      faktfriatemp.PRIS_ENHET faktfriatemp.ANTAL
      faktfriatemp.FAKTTEXT       
      WITH FRAME {&FRAME-NAME}.
      DISPLAY 
      /*   CMB_VAL */
      faktfriatemp.PRIS_ENHET 
      faktfriatemp.ANTAL
      faktfriatemp.FAKTTEXT       
      faktfriatemp.TOTKALK faktfriatemp.TOTALT
      WITH FRAME {&FRAME-NAME}.
      RUN openbdynspec_UI IN brwproc[1].
      ASSIGN      
/*       FILL-IN-SOK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE */
      FILL-IN-OVRK:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      RECT-21:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.     
      ENABLE BRW_ANM BTN_NYANM BTN_UPPANM BTN_BORTANM BTN_KONTO FILL-IN_SFORNAMN FILL-IN_SPERSONALKOD
      WITH FRAME {&FRAME-NAME}.
/*       DISPLAY FILL-IN-SOK WITH FRAME {&FRAME-NAME}. */
      IF NOT AVAILABLE fastanmtemp THEN DO:
         ASSIGN        
         BTN_BORTANM:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         BTN_UPPANM:HIDDEN IN FRAME {&FRAME-NAME} = TRUE         
         BTN_KONTO:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         FILL-IN_SFORNAMN:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
         FILL-IN_SPERSONALKOD:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      END.       
   END.
   ELSE IF RAD_VAL = 5 THEN DO:
      ASSIGN
      faktfriatemp.ENHET = "st"
      faktfriatemp.ANTAL:LABEL = "Antal".
      ENABLE  
      faktfriatemp.PRIS_ENHET faktfriatemp.ANTAL
      faktfriatemp.FAKTTEXT       
      WITH FRAME {&FRAME-NAME}.
      DISPLAY faktfriatemp.PRIS_ENHET 
      faktfriatemp.ANTAL
      faktfriatemp.FAKTTEXT       
      faktfriatemp.TOTKALK faktfriatemp.TOTALT
      WITH FRAME {&FRAME-NAME}.
      
/*       RUN openbdyn_UI IN brwproc[2] (INPUT ""). */
      RUN openbdyn_UI IN brwproc[3] (INPUT "").
      ASSIGN
      FILL-IN-OVRK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      RECT-21:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      
      BRW_HLEV:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      BRW_MTRL:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      btn_back:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      btn_over:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
/*       FILL-IN-SOK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE */
      FILL-IN-BEN:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      FILL-IN-ENR:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      RAD_SOK:HIDDEN IN FRAME {&FRAME-NAME} = FALSE
      CMB_LEV:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      ENABLE BRW_HLEV BRW_MTRL FILL-IN-ENR CMB_LEV FILL-IN-BEN btn_over btn_back
      WITH FRAME {&FRAME-NAME}.
/*       DISPLAY FILL-IN-SOK WITH FRAME {&FRAME-NAME}. */
   END. 
   IF RAD_VAL = 7 THEN DO:
      IF nya = FALSE THEN DO:
         FIND FIRST ekalkkattemp WHERE ekalkkattemp.ROWFRI = faktfriatemp.ROWFRI NO-LOCK NO-ERROR.
         IF NOT AVAILABLE ekalkkattemp THEN DO:
            CREATE ekalkkattemp.
            ASSIGN
            ekalkkattemp.ROWFRIA = faktfriatemp.ROWFRI     
            ekalkkattemp.ANTAL   = faktfriatemp.ANTAL       
            ekalkkattemp.VINAMN  = faktfriatemp.FAKTTEXT    
            ekalkkattemp.PRIS    = faktfriatemp.PRIS_ENHET.  
            ekalkkattemp.TOTFAKT = ekalkkattemp.ANTAL * ekalkkattemp.PRIS.
            RUN fritot_UI.
         END.
         ENABLE BRW_VFRIP WITH FRAME {&FRAME-NAME}.
      END.
      ELSE DO:
         ASSIGN
         BRW_VFRIP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
         BRW_FRIP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
         btn_back:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
         btn_over:HIDDEN IN FRAME {&FRAME-NAME} = FALSE. 
         ENABLE BRW_FRIP BRW_VFRIP btn_over btn_back
         WITH FRAME {&FRAME-NAME}.
         RUN openbdynspec_UI IN brwproc[5].
      END.
      RUN openbdynspec_UI IN brwproc[6].            
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION runda Dialog-Frame 
FUNCTION runda RETURNS DECIMAL
  ( INPUT varedin AS DECIMAL) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
 
  RETURN ROUND(varedin,0).    /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

