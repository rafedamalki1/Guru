&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{WHANDLTEMP.I}
&Scoped-define NEW   
{GLOBVAR2DEL1.I}
DEFINE VARIABLE schakth AS HANDLE NO-UNDO.
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{KONSTRMTRL.I}
{SCADMIN.I}
/*kalkkoder*/
{LOPTEMP.I}
{PTEMP.I}
{FASTKALKTEMP.I}

&Scoped-define SHARED SHARED
/*DEFINE SHARED TEMP-TABLE extravaldfasttemp NO-UNDO LIKE valdfasttemp.*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_FOR

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tempforlagg temphandkopp temphandelse ~
tempkabel tempkkopp tempkonst loptemp ptemp tempsamforlagg fastkalktemp ~
tempytbelagg

/* Definitions for BROWSE BRW_FOR                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_FOR tempforlagg.FORLAGG ~
tempforlagg.BENAMNING tempforlagg.DJUP tempforlagg.SAM tempforlagg.ROR ~
tempforlagg.MARK tempforlagg.FAKTOR tempforlagg.TILLAGG tempforlagg.BORT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_FOR 
&Scoped-define QUERY-STRING-BRW_FOR FOR EACH tempforlagg NO-LOCK ~
    BY tempforlagg.FORLAGG
&Scoped-define OPEN-QUERY-BRW_FOR OPEN QUERY BRW_FOR FOR EACH tempforlagg NO-LOCK ~
    BY tempforlagg.FORLAGG.
&Scoped-define TABLES-IN-QUERY-BRW_FOR tempforlagg
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_FOR tempforlagg


/* Definitions for BROWSE BRW_HAND                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_HAND temphandkopp.ID temphandkopp.TYP ~
temphandkopp.Ben temphandkopp.FORLAGG 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_HAND 
&Scoped-define QUERY-STRING-BRW_HAND FOR EACH temphandkopp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_HAND OPEN QUERY BRW_HAND FOR EACH temphandkopp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_HAND temphandkopp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_HAND temphandkopp


/* Definitions for BROWSE BRW_HANDELSE                                  */
&Scoped-define FIELDS-IN-QUERY-BRW_HANDELSE temphandelse.BENAMNING ~
temphandelse.ENHET temphandelse.BORT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_HANDELSE 
&Scoped-define QUERY-STRING-BRW_HANDELSE FOR EACH temphandelse NO-LOCK
&Scoped-define OPEN-QUERY-BRW_HANDELSE OPEN QUERY BRW_HANDELSE FOR EACH temphandelse NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_HANDELSE temphandelse
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_HANDELSE temphandelse


/* Definitions for BROWSE BRW_KAB                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_KAB tempkabel.KABEL tempkabel.DIAMETER 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KAB tempkabel.DIAMETER 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_KAB tempkabel
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_KAB tempkabel
&Scoped-define QUERY-STRING-BRW_KAB FOR EACH tempkabel NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KAB OPEN QUERY BRW_KAB FOR EACH tempkabel NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KAB tempkabel
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KAB tempkabel


/* Definitions for BROWSE BRW_KKOPP                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_KKOPP tempkkopp.ARBKOD tempkkopp.LOPNR ~
tempkkopp.ANTAL tempkkopp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KKOPP tempkkopp.ANTAL 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_KKOPP tempkkopp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_KKOPP tempkkopp
&Scoped-define QUERY-STRING-BRW_KKOPP FOR EACH tempkkopp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KKOPP OPEN QUERY BRW_KKOPP FOR EACH tempkkopp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KKOPP tempkkopp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KKOPP tempkkopp


/* Definitions for BROWSE BRW_KON                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_KON tempkonst.BENAMNING ~
tempkonst.KTYPKOD tempkonst.ORDNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KON 
&Scoped-define QUERY-STRING-BRW_KON FOR EACH tempkonst NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KON OPEN QUERY BRW_KON FOR EACH tempkonst NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KON tempkonst
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KON tempkonst


/* Definitions for BROWSE BRW_LOP                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_LOP loptemp.LOPNR loptemp.ENHET ~
loptemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_LOP 
&Scoped-define QUERY-STRING-BRW_LOP FOR EACH loptemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_LOP OPEN QUERY BRW_LOP FOR EACH loptemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_LOP loptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_LOP loptemp


/* Definitions for BROWSE BRW_P                                         */
&Scoped-define FIELDS-IN-QUERY-BRW_P ptemp.ARBKOD ptemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_P 
&Scoped-define QUERY-STRING-BRW_P FOR EACH ptemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_P OPEN QUERY BRW_P FOR EACH ptemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_P ptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_P ptemp


/* Definitions for BROWSE BRW_SAM                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_SAM tempsamforlagg.KOD ~
tempsamforlagg.BENAMNING tempsamforlagg.DIAMETER 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_SAM 
&Scoped-define QUERY-STRING-BRW_SAM FOR EACH tempsamforlagg NO-LOCK ~
    BY tempsamforlagg.KOD
&Scoped-define OPEN-QUERY-BRW_SAM OPEN QUERY BRW_SAM FOR EACH tempsamforlagg NO-LOCK ~
    BY tempsamforlagg.KOD.
&Scoped-define TABLES-IN-QUERY-BRW_SAM tempsamforlagg
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_SAM tempsamforlagg


/* Definitions for BROWSE BRW_VALKOD                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_VALKOD fastkalktemp.ARBKOD ~
fastkalktemp.LOPNR fastkalktemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VALKOD 
&Scoped-define QUERY-STRING-BRW_VALKOD FOR EACH fastkalktemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VALKOD OPEN QUERY BRW_VALKOD FOR EACH fastkalktemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VALKOD fastkalktemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VALKOD fastkalktemp


/* Definitions for BROWSE BRW_YT                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_YT tempytbelagg.YTBELAGG ~
tempytbelagg.BORT 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_YT 
&Scoped-define QUERY-STRING-BRW_YT FOR EACH tempytbelagg NO-LOCK
&Scoped-define OPEN-QUERY-BRW_YT OPEN QUERY BRW_YT FOR EACH tempytbelagg NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_YT tempytbelagg
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_YT tempytbelagg


/* Definitions for FRAME FRAME-FORLSATT                                 */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-FORLSATT ~
    ~{&OPEN-QUERY-BRW_FOR}~
    ~{&OPEN-QUERY-BRW_HANDELSE}~
    ~{&OPEN-QUERY-BRW_SAM}~
    ~{&OPEN-QUERY-BRW_YT}

/* Definitions for FRAME FRAME-KABLAR                                   */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-KABLAR ~
    ~{&OPEN-QUERY-BRW_KAB}

/* Definitions for FRAME FRAME-KKOPP                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-KKOPP ~
    ~{&OPEN-QUERY-BRW_HAND}~
    ~{&OPEN-QUERY-BRW_KKOPP}~
    ~{&OPEN-QUERY-BRW_VALKOD}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FBTN_HANDEL RECT-MENY FBTN_FORLSATT ~
FBTN_KKOPP BTN_AVSLUTA FBTN_YT FBTN_KAB FBTN_SAM 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVSLUTA 
     LABEL "Avsluta" 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_FORLSATT  NO-FOCUS
     LABEL "Forlsätt" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON FBTN_HANDEL  NO-FOCUS
     LABEL "Händelser" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON FBTN_KAB  NO-FOCUS
     LABEL "Kablar" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON FBTN_KKOPP  NO-FOCUS
     LABEL "Kkopp" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON FBTN_SAM  NO-FOCUS
     LABEL "Samförlägg" 
     SIZE 7.25 BY 2.33.

DEFINE BUTTON FBTN_YT  NO-FOCUS
     LABEL "Ytbelägg" 
     SIZE 7.25 BY 2.33.

DEFINE RECTANGLE RECT-MENY
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 120.25 BY .08.

DEFINE BUTTON BTN_AND 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_MORDNER 
     IMAGE-UP FILE "BILDER\pilner":U
     LABEL "Ner" 
     SIZE 4 BY 1.5 TOOLTIP "Flytta ner".

DEFINE BUTTON BTN_MORDUPP 
     IMAGE-UP FILE "BILDER\pilupp":U
     LABEL "" 
     SIZE 4 BY 1.5 TOOLTIP "Flytta upp".

DEFINE BUTTON BTN_NY 
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE BUTTON AddKabelButton 
     IMAGE-UP FILE "bilder/next-u.bmp":U
     LABEL "->" 
     SIZE 4 BY 1.21.

DEFINE BUTTON BTN_BORTKAB 
     LABEL "Ta Bort" 
     SIZE 12 BY 1.

DEFINE VARIABLE CMB_VAL2 AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 28.5 BY 1 NO-UNDO.

DEFINE BUTTON BTN_KOPPLA 
     LABEL "V" 
     SIZE 4 BY 1.21.

DEFINE BUTTON BTN_OKOPPLA 
     LABEL "^" 
     SIZE 4 BY 1.21.

DEFINE BUTTON BTN_OVALKOD 
     LABEL "<" 
     SIZE 4 BY 1.25.

DEFINE BUTTON BTN_VALKOD 
     LABEL "V" 
     SIZE 4 BY 1.21.

DEFINE VARIABLE FILL-IN-BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-VKOD AS CHARACTER FORMAT "X(256)":U 
     LABEL "Arbkod" 
     VIEW-AS FILL-IN 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-VLOP AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Löpnr" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_HTYP AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Punkt Händelser", 1,
"Förl.Händelser", 5,
"Ytbeläggningar", 2,
"Förläggningssätt", 3
     SIZE 19.5 BY 4 NO-UNDO.

DEFINE VARIABLE RAD_TYP AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "", 1
     SIZE 40 BY 1.25 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_FOR FOR 
      tempforlagg SCROLLING.

DEFINE QUERY BRW_HAND FOR 
      temphandkopp SCROLLING.

DEFINE QUERY BRW_HANDELSE FOR 
      temphandelse SCROLLING.

DEFINE QUERY BRW_KAB FOR 
      tempkabel SCROLLING.

DEFINE QUERY BRW_KKOPP FOR 
      tempkkopp SCROLLING.

DEFINE QUERY BRW_KON FOR 
      tempkonst SCROLLING.

DEFINE QUERY BRW_LOP FOR 
      loptemp SCROLLING.

DEFINE QUERY BRW_P FOR 
      ptemp SCROLLING.

DEFINE QUERY BRW_SAM FOR 
      tempsamforlagg SCROLLING.

DEFINE QUERY BRW_VALKOD FOR 
      fastkalktemp SCROLLING.

DEFINE QUERY BRW_YT FOR 
      tempytbelagg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_FOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_FOR C-Win _STRUCTURED
  QUERY BRW_FOR NO-LOCK DISPLAY
      tempforlagg.FORLAGG COLUMN-LABEL "Kod" FORMAT "X(256)":U
            WIDTH 19.5
      tempforlagg.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 23.5
      tempforlagg.DJUP COLUMN-LABEL "Djup" FORMAT "->,>>>,>>9":U
            WIDTH 6.5
      tempforlagg.SAM COLUMN-LABEL "Sam" FORMAT "Ja/Nej":U
      tempforlagg.ROR COLUMN-LABEL "Rör" FORMAT "Ja/Nej":U
      tempforlagg.MARK COLUMN-LABEL "Svår Mark" FORMAT "Ja/Nej":U
      tempforlagg.FAKTOR COLUMN-LABEL "Faktor" FORMAT "->>,>>9.99":U
      tempforlagg.TILLAGG COLUMN-LABEL "Tillägg" FORMAT "->,>>>,>>9":U
      tempforlagg.BORT COLUMN-LABEL "Borttagen" FORMAT "Ja/Nej":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 91 BY 21.21
         TITLE "Förläggningssätt" ROW-HEIGHT-CHARS .29.

DEFINE BROWSE BRW_HAND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_HAND C-Win _STRUCTURED
  QUERY BRW_HAND NO-LOCK DISPLAY
      temphandkopp.ID FORMAT "->,>>>,>>9":U WIDTH .5
      temphandkopp.TYP FORMAT "X(8)":U WIDTH 1.13
      temphandkopp.Ben COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 20
      temphandkopp.FORLAGG COLUMN-LABEL "Kod" FORMAT "X(256)":U
            WIDTH 12
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 36 BY 6.5
         TITLE "Händelse/Ytbeläggningar/Förläggningssätt" FIT-LAST-COLUMN.

DEFINE BROWSE BRW_HANDELSE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_HANDELSE C-Win _STRUCTURED
  QUERY BRW_HANDELSE NO-LOCK DISPLAY
      temphandelse.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 40
      temphandelse.ENHET COLUMN-LABEL "Enhet" FORMAT "X(8)":U WIDTH 10
      temphandelse.BORT COLUMN-LABEL "Borttagen" FORMAT "Ja/Nej":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 47.88 BY 21.25
         TITLE "Händelser" FIT-LAST-COLUMN.

DEFINE BROWSE BRW_KAB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KAB C-Win _STRUCTURED
  QUERY BRW_KAB NO-LOCK DISPLAY
      tempkabel.KABEL COLUMN-LABEL "Kabel" FORMAT "X(256)":U WIDTH 34.63
      tempkabel.DIAMETER COLUMN-LABEL "Diameter" FORMAT "->,>>>,>>9":U
  ENABLE
      tempkabel.DIAMETER
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 47.5 BY 19.25
         TITLE "Kablar" FIT-LAST-COLUMN.

DEFINE BROWSE BRW_KKOPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KKOPP C-Win _STRUCTURED
  QUERY BRW_KKOPP NO-LOCK DISPLAY
      tempkkopp.ARBKOD COLUMN-LABEL "Kod" FORMAT "X(8)":U WIDTH 6.75
      tempkkopp.LOPNR COLUMN-LABEL "Löpnr" FORMAT "->,>>>,>>9":U
      tempkkopp.ANTAL COLUMN-LABEL "Antal" FORMAT "->>,>>9.99":U
      tempkkopp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
  ENABLE
      tempkkopp.ANTAL
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-SCROLLBAR-VERTICAL SIZE 41 BY 6.5
         TITLE "Kopplade koder" FIT-LAST-COLUMN.

DEFINE BROWSE BRW_KON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KON C-Win _STRUCTURED
  QUERY BRW_KON NO-LOCK DISPLAY
      tempkonst.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 24
      tempkonst.KTYPKOD COLUMN-LABEL "Kod" FORMAT "X(256)":U WIDTH 13.5
      tempkonst.ORDNING COLUMN-LABEL "Ordning" FORMAT ">>>>>>>>>9":U
            WIDTH 7.5
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 49 BY 19.17
         TITLE "Konstruktioner".

DEFINE BROWSE BRW_LOP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_LOP C-Win _STRUCTURED
  QUERY BRW_LOP NO-LOCK DISPLAY
      loptemp.LOPNR COLUMN-LABEL "Löpnr" FORMAT ">>>":U COLUMN-FGCOLOR 12
      loptemp.ENHET COLUMN-LABEL "Enh" FORMAT "X(3)":U
      loptemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 30
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 42 BY 7
         TITLE "Löpnr kopplade till arbetskoden" ROW-HEIGHT-CHARS .54.

DEFINE BROWSE BRW_P
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_P C-Win _STRUCTURED
  QUERY BRW_P NO-LOCK DISPLAY
      ptemp.ARBKOD COLUMN-LABEL "Kod" FORMAT "X(5)":U COLUMN-FGCOLOR 12
      ptemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 35
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 42 BY 7
         TITLE "Arbetskoder".

DEFINE BROWSE BRW_SAM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_SAM C-Win _STRUCTURED
  QUERY BRW_SAM NO-LOCK DISPLAY
      tempsamforlagg.KOD COLUMN-LABEL "Kod" FORMAT "X(8)":U WIDTH 11
      tempsamforlagg.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 31
      tempsamforlagg.DIAMETER COLUMN-LABEL "Diameter" FORMAT "->,>>>,>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 56 BY 21.25
         TITLE "Samförläggning".

DEFINE BROWSE BRW_VALKOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VALKOD C-Win _STRUCTURED
  QUERY BRW_VALKOD NO-LOCK DISPLAY
      fastkalktemp.ARBKOD COLUMN-LABEL "Kod" FORMAT "X(5)":U WIDTH 9.5
      fastkalktemp.LOPNR COLUMN-LABEL "Löpnr" FORMAT ">>>":U WIDTH 7
      fastkalktemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(256)":U
            WIDTH 31
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 50.5 BY 7.25
         TITLE "Valda koder" ROW-HEIGHT-CHARS .54.

DEFINE BROWSE BRW_YT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_YT C-Win _STRUCTURED
  QUERY BRW_YT NO-LOCK DISPLAY
      tempytbelagg.YTBELAGG COLUMN-LABEL "Ytbeläggning" FORMAT "X(256)":U
      tempytbelagg.BORT COLUMN-LABEL "Borttagen" FORMAT "Ja/Nej":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 49.63 BY 21.25
         TITLE "Ytbeläggning" ROW-HEIGHT-CHARS 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FBTN_HANDEL AT ROW 1 COL 90.75
     FBTN_FORLSATT AT ROW 1 COL 65.5
     FBTN_KKOPP AT ROW 1 COL 80.5
     BTN_AVSLUTA AT ROW 28 COL 106.75
     FBTN_YT AT ROW 1 COL 19.5
     FBTN_KAB AT ROW 1 COL 1
     FBTN_SAM AT ROW 1 COL 36.5
     RECT-MENY AT ROW 3.38 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 120.25 BY 28.42.

DEFINE FRAME FRAME-FORLSATT
     BRW_SAM AT ROW 3 COL 22.5
     BRW_YT AT ROW 3 COL 25.63
     BRW_HANDELSE AT ROW 3 COL 27.25
     BRW_FOR AT ROW 3.04 COL 5
     BTN_MORDUPP AT ROW 11.33 COL 96.88
     BTN_MORDNER AT ROW 14.25 COL 96.88
     BTN_NY AT ROW 24.75 COL 29
     BTN_AND AT ROW 24.75 COL 43.75
     BTN_BORT AT ROW 24.75 COL 58.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.5 ROW 3.5
         SIZE 105 BY 25.75.

DEFINE FRAME FRAME-KKOPP
     RAD_TYP AT ROW 1 COL 4 NO-LABEL
     BRW_P AT ROW 2.75 COL 3
     BRW_VALKOD AT ROW 9.25 COL 54
     FILL-IN-BEN AT ROW 10.25 COL 11.88 COLON-ALIGNED
     BRW_LOP AT ROW 11.5 COL 3
     BTN_VALKOD AT ROW 12 COL 48
     BTN_OVALKOD AT ROW 13.75 COL 48
     BTN_OKOPPLA AT ROW 17.25 COL 73
     BTN_KOPPLA AT ROW 17.25 COL 83.5
     FILL-IN-VKOD AT ROW 19 COL 9 COLON-ALIGNED
     BRW_HAND AT ROW 19 COL 27
     FILL-IN-VLOP AT ROW 19 COL 20.13
     BRW_KKOPP AT ROW 19.04 COL 63.5
     RAD_HTYP AT ROW 21.25 COL 4.38 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.42
         SIZE 104.5 BY 25.75.

DEFINE FRAME FRAME-KABLAR
     CMB_VAL2 AT ROW 3.13 COL 1.5 NO-LABEL
     BRW_KON AT ROW 5 COL 1.5
     BRW_KAB AT ROW 5 COL 55.5
     AddKabelButton AT ROW 12 COL 51
     BTN_BORTKAB AT ROW 24.5 COL 74.13
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 3.42
         SIZE 104.5 BY 25.83.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: fastkalktemp T "?" NO-UNDO temp-db fastkalktemp
      TABLE: konsttemp T "?" NO-UNDO temp-db konsttemp
      TABLE: kskyddtemp T "?" NO-UNDO temp-db kskyddtemp
      TABLE: loptemp T "?" NO-UNDO temp-db loptemp
      TABLE: ptemp T "?" NO-UNDO temp-db ptemp
      TABLE: tempforlagg T "?" NO-UNDO temp-db tempforlagg
      TABLE: temphandelse T "?" NO-UNDO temp-db temphandelse
      TABLE: temphandkopp T "?" NO-UNDO temp-db temphandkopp
      TABLE: tempkabel T "?" NO-UNDO temp-db tempkabel
      TABLE: tempkkopp T "?" NO-UNDO temp-db tempkkopp
      TABLE: tempkonst T "?" NO-UNDO temp-db tempkonst
      TABLE: tempsamforlagg T "?" NO-UNDO temp-db tempsamforlagg
      TABLE: tempytbelagg T "?" NO-UNDO temp-db tempytbelagg
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 28.46
         WIDTH              = 120.38
         MAX-HEIGHT         = 28.83
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 28.83
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
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-FORLSATT:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-KABLAR:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME FRAME-KKOPP:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME FRAME-FORLSATT
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_SAM 1 FRAME-FORLSATT */
/* BROWSE-TAB BRW_YT BRW_SAM FRAME-FORLSATT */
/* BROWSE-TAB BRW_HANDELSE BRW_YT FRAME-FORLSATT */
/* BROWSE-TAB BRW_FOR BRW_HANDELSE FRAME-FORLSATT */
ASSIGN 
       FRAME FRAME-FORLSATT:HIDDEN           = TRUE.

ASSIGN 
       BRW_FOR:HIDDEN  IN FRAME FRAME-FORLSATT                = TRUE
       BRW_FOR:MAX-DATA-GUESS IN FRAME FRAME-FORLSATT         = 1000
       BRW_FOR:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-FORLSATT = TRUE
       BRW_FOR:COLUMN-RESIZABLE IN FRAME FRAME-FORLSATT       = TRUE.

ASSIGN 
       BRW_SAM:HIDDEN  IN FRAME FRAME-FORLSATT                = TRUE
       BRW_SAM:MAX-DATA-GUESS IN FRAME FRAME-FORLSATT         = 1000
       BRW_SAM:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-FORLSATT = TRUE
       BRW_SAM:COLUMN-RESIZABLE IN FRAME FRAME-FORLSATT       = TRUE.

ASSIGN 
       BRW_YT:HIDDEN  IN FRAME FRAME-FORLSATT                = TRUE
       BRW_YT:MAX-DATA-GUESS IN FRAME FRAME-FORLSATT         = 1000
       BRW_YT:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-FORLSATT = TRUE
       BRW_YT:COLUMN-RESIZABLE IN FRAME FRAME-FORLSATT       = TRUE.

ASSIGN 
       BTN_MORDNER:HIDDEN IN FRAME FRAME-FORLSATT           = TRUE.

/* SETTINGS FOR FRAME FRAME-KABLAR
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_KON CMB_VAL2 FRAME-KABLAR */
/* BROWSE-TAB BRW_KAB BRW_KON FRAME-KABLAR */
ASSIGN 
       FRAME FRAME-KABLAR:HIDDEN           = TRUE.

ASSIGN 
       BRW_KON:HIDDEN  IN FRAME FRAME-KABLAR                = TRUE
       BRW_KON:MAX-DATA-GUESS IN FRAME FRAME-KABLAR         = 300.

/* SETTINGS FOR COMBO-BOX CMB_VAL2 IN FRAME FRAME-KABLAR
   ALIGN-L                                                              */
/* SETTINGS FOR FRAME FRAME-KKOPP
                                                                        */
/* BROWSE-TAB BRW_P RAD_TYP FRAME-KKOPP */
/* BROWSE-TAB BRW_VALKOD BRW_P FRAME-KKOPP */
/* BROWSE-TAB BRW_LOP FILL-IN-BEN FRAME-KKOPP */
/* BROWSE-TAB BRW_HAND FILL-IN-VKOD FRAME-KKOPP */
/* BROWSE-TAB BRW_KKOPP FILL-IN-VLOP FRAME-KKOPP */
ASSIGN 
       temphandkopp.ID:VISIBLE IN BROWSE BRW_HAND = FALSE
       temphandkopp.TYP:VISIBLE IN BROWSE BRW_HAND = FALSE.

ASSIGN 
       BRW_LOP:MAX-DATA-GUESS IN FRAME FRAME-KKOPP         = 1000
       BRW_LOP:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-KKOPP = TRUE
       BRW_LOP:COLUMN-RESIZABLE IN FRAME FRAME-KKOPP       = TRUE.

ASSIGN 
       BRW_P:MAX-DATA-GUESS IN FRAME FRAME-KKOPP         = 1000
       BRW_P:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-KKOPP = TRUE
       BRW_P:COLUMN-RESIZABLE IN FRAME FRAME-KKOPP       = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-VKOD IN FRAME FRAME-KKOPP
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-VKOD:HIDDEN IN FRAME FRAME-KKOPP           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-VLOP IN FRAME FRAME-KKOPP
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       FILL-IN-VLOP:HIDDEN IN FRAME FRAME-KKOPP           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_FOR
/* Query rebuild information for BROWSE BRW_FOR
     _TblList          = "Temp-Tables.tempforlagg"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.tempforlagg.FORLAGG|yes"
     _FldNameList[1]   > Temp-Tables.tempforlagg.FORLAGG
"FORLAGG" "Kod" "X(256)" "character" ? ? ? ? ? ? no ? no no "19.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.tempforlagg.BENAMNING
"BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "23.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.tempforlagg.DJUP
"DJUP" "Djup" ? "integer" ? ? ? ? ? ? no ? no no "6.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.tempforlagg.SAM
"SAM" "Sam" "Ja/Nej" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.tempforlagg.ROR
"ROR" "Rör" "Ja/Nej" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.tempforlagg.MARK
"MARK" "Svår Mark" "Ja/Nej" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.tempforlagg.FAKTOR
"FAKTOR" "Faktor" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.tempforlagg.TILLAGG
"TILLAGG" "Tillägg" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.tempforlagg.BORT
"BORT" "Borttagen" ? "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_FOR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_HAND
/* Query rebuild information for BROWSE BRW_HAND
     _TblList          = "Temp-Tables.temphandkopp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.temphandkopp.ID
"temphandkopp.ID" ? ? "integer" ? ? ? ? ? ? no ? no no ".5" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.temphandkopp.TYP
"temphandkopp.TYP" ? ? "character" ? ? ? ? ? ? no ? no no "1.13" no no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.temphandkopp.Ben
"temphandkopp.Ben" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.temphandkopp.FORLAGG
"temphandkopp.FORLAGG" "Kod" "X(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_HAND */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_HANDELSE
/* Query rebuild information for BROWSE BRW_HANDELSE
     _TblList          = "Temp-Tables.temphandelse"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.temphandelse.BENAMNING
"BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.temphandelse.ENHET
"ENHET" "Enhet" ? "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.temphandelse.BORT
"BORT" "Borttagen" "Ja/Nej" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_HANDELSE */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KAB
/* Query rebuild information for BROWSE BRW_KAB
     _TblList          = "Temp-Tables.tempkabel"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.tempkabel.KABEL
"tempkabel.KABEL" "Kabel" "X(256)" "character" ? ? ? ? ? ? no ? no no "34.63" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.tempkabel.DIAMETER
"tempkabel.DIAMETER" "Diameter" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_KAB */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KKOPP
/* Query rebuild information for BROWSE BRW_KKOPP
     _TblList          = "Temp-Tables.tempkkopp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.tempkkopp.ARBKOD
"tempkkopp.ARBKOD" "Kod" ? "character" ? ? ? ? ? ? no ? no no "6.75" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.tempkkopp.LOPNR
"tempkkopp.LOPNR" "Löpnr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.tempkkopp.ANTAL
"tempkkopp.ANTAL" "Antal" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.tempkkopp.BENAMNING
"tempkkopp.BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_KKOPP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KON
/* Query rebuild information for BROWSE BRW_KON
     _TblList          = "Temp-Tables.tempkonst"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.tempkonst.BENAMNING
"tempkonst.BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "24" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.tempkonst.KTYPKOD
"tempkonst.KTYPKOD" "Kod" "X(256)" "character" ? ? ? ? ? ? no ? no no "13.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.tempkonst.ORDNING
"tempkonst.ORDNING" "Ordning" ">>>>>>>>>9" "integer" ? ? ? ? ? ? no ? no no "7.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KON */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_LOP
/* Query rebuild information for BROWSE BRW_LOP
     _TblList          = "Temp-Tables.loptemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.loptemp.LOPNR
"loptemp.LOPNR" "Löpnr" ? "integer" ? 12 ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.loptemp.ENHET
"loptemp.ENHET" "Enh" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.loptemp.BENAMNING
"loptemp.BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_LOP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_P
/* Query rebuild information for BROWSE BRW_P
     _TblList          = "Temp-Tables.ptemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.ptemp.ARBKOD
"ptemp.ARBKOD" "Kod" ? "character" ? 12 ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.ptemp.BENAMNING
"ptemp.BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "35" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_P */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_SAM
/* Query rebuild information for BROWSE BRW_SAM
     _TblList          = "Temp-Tables.tempsamforlagg"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.tempsamforlagg.KOD|yes"
     _FldNameList[1]   > Temp-Tables.tempsamforlagg.KOD
"tempsamforlagg.KOD" "Kod" ? "character" ? ? ? ? ? ? no ? no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.tempsamforlagg.BENAMNING
"tempsamforlagg.BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "31" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.tempsamforlagg.DIAMETER
"tempsamforlagg.DIAMETER" "Diameter" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_SAM */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VALKOD
/* Query rebuild information for BROWSE BRW_VALKOD
     _TblList          = "Temp-Tables.fastkalktemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.fastkalktemp.ARBKOD
"fastkalktemp.ARBKOD" "Kod" ? "character" ? ? ? ? ? ? no ? no no "9.5" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.fastkalktemp.LOPNR
"fastkalktemp.LOPNR" "Löpnr" ? "integer" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.fastkalktemp.BENAMNING
"fastkalktemp.BENAMNING" "Benämning" "X(256)" "character" ? ? ? ? ? ? no ? no no "31" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_VALKOD */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_YT
/* Query rebuild information for BROWSE BRW_YT
     _TblList          = "Temp-Tables.tempytbelagg"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.tempytbelagg.YTBELAGG
"YTBELAGG" "Ytbeläggning" "X(256)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.tempytbelagg.BORT
"BORT" "Borttagen" "Ja/Nej" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_YT */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_FOR
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


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

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {WIN_M_START.I}
   {muswait.i}
   /*RAD_VAL = 1.*/ /*ccccc*/
   EMPTY TEMP-TABLE fastkalktemp.
   RUN enable_UI.
   {FRMSIZEF.I}
   {musarrow.i}
   RUN wstart_UI.
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE avsluta_UI C-Win 
PROCEDURE avsluta_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   IF VALID-HANDLE(schakth) THEN DO:
      RUN borthand_UI IN schakth.
      DELETE PROCEDURE schakth NO-ERROR.      
   END.
   APPLY "CLOSE" TO THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE brwhand_UI C-Win 
PROCEDURE brwhand_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER radhtyp AS INTEGER NO-UNDO.
   temphandkopp.FORLAGG:VISIBLE IN BROWSE BRW_HAND = FALSE.
   temphandkopp.BEN:LABEL = "Benämning".
   temphandkopp.BEN:WIDTH = 33.
   IF radhtyp = 3 THEN DO:
      ASSIGN
      temphandkopp.BEN:LABEL = "Kod"
      temphandkopp.FORLAGG:LABEL = "Benämning"
      temphandkopp.FORLAGG:VISIBLE IN BROWSE BRW_HAND = TRUE.
      temphandkopp.BEN:WIDTH = 8.
      temphandkopp.FORLAGG:WIDTH = 24.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  ENABLE FBTN_HANDEL RECT-MENY FBTN_FORLSATT FBTN_KKOPP BTN_AVSLUTA FBTN_YT 
         FBTN_KAB FBTN_SAM 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY CMB_VAL2 
      WITH FRAME FRAME-KABLAR IN WINDOW C-Win.
  ENABLE CMB_VAL2 BRW_KON BRW_KAB AddKabelButton BTN_BORTKAB 
      WITH FRAME FRAME-KABLAR IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-KABLAR}
  DISPLAY RAD_TYP FILL-IN-BEN RAD_HTYP 
      WITH FRAME FRAME-KKOPP IN WINDOW C-Win.
  ENABLE RAD_TYP BRW_P BRW_VALKOD FILL-IN-BEN BRW_LOP BTN_VALKOD BTN_OVALKOD 
         BTN_OKOPPLA BTN_KOPPLA BRW_HAND BRW_KKOPP RAD_HTYP 
      WITH FRAME FRAME-KKOPP IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-KKOPP}
  ENABLE BRW_SAM BRW_YT BRW_HANDELSE BRW_FOR BTN_MORDUPP BTN_MORDNER BTN_NY 
         BTN_AND BTN_BORT 
      WITH FRAME FRAME-FORLSATT IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-FORLSATT}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE whandle_UI C-Win 
PROCEDURE whandle_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT-OUTPUT PARAMETER ordnr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER ordh AS HANDLE NO-UNDO.
   ASSIGN
   whandltemp.WF[ordnr] = ordh.
   ordnr = ordnr + 1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE wstart_UI C-Win 
PROCEDURE wstart_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/      
   DEFINE VARIABLE ordningnr AS INTEGER NO-UNDO.
   EMPTY TEMP-TABLE whandltemp NO-ERROR. 
   CREATE whandltemp.
   ordningnr = 1.   
   RUN whandle_UI (INPUT-OUTPUT ordningnr,C-WIN:HANDLE).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-BEN:HANDLE IN FRAME FRAME-KKOPP).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-VKOD:HANDLE IN FRAME FRAME-KKOPP).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FILL-IN-VLOP:HANDLE IN FRAME FRAME-KKOPP).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,RAD_HTYP:HANDLE IN FRAME FRAME-KKOPP).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr, BTN_OKOPPLA:HANDLE IN FRAME FRAME-KKOPP).
   RUN whandle_UI (INPUT-OUTPUT ordningnr, BTN_KOPPLA:HANDLE IN FRAME FRAME-KKOPP).
   RUN whandle_UI (INPUT-OUTPUT ordningnr, BTN_VALKOD:HANDLE IN FRAME FRAME-KKOPP).
   RUN whandle_UI (INPUT-OUTPUT ordningnr, BTN_OVALKOD:HANDLE IN FRAME FRAME-KKOPP).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,RAD_TYP:HANDLE IN FRAME FRAME-KKOPP).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FBTN_HANDEL:HANDLE IN FRAME DEFAULT-FRAME).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FBTN_KKOPP:HANDLE IN FRAME DEFAULT-FRAME).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FBTN_FORLSATT:HANDLE IN FRAME DEFAULT-FRAME).          
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FBTN_YT:HANDLE IN FRAME DEFAULT-FRAME).
   RUN whandle_UI (INPUT-OUTPUT ordningnr,FBTN_SAM:HANDLE IN FRAME DEFAULT-FRAME).
   RUN whandle_UI (INPUT-OUTPUT ordningnr, FBTN_KAB:HANDLE IN FRAME DEFAULT-FRAME).
   RUN whandle_UI (INPUT-OUTPUT ordningnr, FRAME FRAME-KKOPP:HANDLE).
   RUN whandle_UI (INPUT-OUTPUT ordningnr, FRAME FRAME-KABLAR:HANDLE).
   RUN whandle_UI (INPUT-OUTPUT ordningnr, FRAME FRAME-FORLSATT:HANDLE).
   RUN whandle_UI (INPUT-OUTPUT ordningnr, CMB_VAL2:HANDLE IN FRAME FRAME-KABLAR).
   RUN whandle_UI (INPUT-OUTPUT ordningnr, BRW_VALKOD:HANDLE IN FRAME FRAME-KKOPP).
   RUN whandle_UI (INPUT-OUTPUT ordningnr, BRW_HAND:HANDLE IN FRAME FRAME-KKOPP).
   RUN whandle_UI (INPUT-OUTPUT ordningnr, BRW_KKOPP:HANDLE IN FRAME FRAME-KKOPP).
   RUN whandle_UI (INPUT-OUTPUT ordningnr, BRW_P:HANDLE IN FRAME FRAME-KKOPP).
   RUN whandle_UI (INPUT-OUTPUT ordningnr, BRW_LOP:HANDLE IN FRAME FRAME-KKOPP).
   RUN whandle_UI (INPUT-OUTPUT ordningnr, BRW_KON:HANDLE IN FRAME FRAME-KABLAR).
   RUN whandle_UI (INPUT-OUTPUT ordningnr, BRW_KAB:HANDLE IN FRAME FRAME-KABLAR).
   RUN whandle_UI (INPUT-OUTPUT ordningnr, BRW_YT:HANDLE IN FRAME FRAME-FORLSATT).
   RUN whandle_UI (INPUT-OUTPUT ordningnr, BRW_SAM:HANDLE IN FRAME FRAME-FORLSATT).
   RUN whandle_UI (INPUT-OUTPUT ordningnr, BRW_FOR:HANDLE IN FRAME FRAME-FORLSATT).
   RUN whandle_UI (INPUT-OUTPUT ordningnr, BRW_HANDELSE:HANDLE IN FRAME FRAME-FORLSATT).
   RUN whandle_UI (INPUT-OUTPUT ordningnr, AddKabelButton:HANDLE IN FRAME FRAME-KABLAR).
   RUN whandle_UI (INPUT-OUTPUT ordningnr, BTN_NY:HANDLE IN FRAME FRAME-FORLSATT).
   RUN whandle_UI (INPUT-OUTPUT ordningnr, BTN_BORT:HANDLE IN FRAME FRAME-FORLSATT).
   RUN whandle_UI (INPUT-OUTPUT ordningnr, BTN_AND:HANDLE IN FRAME FRAME-FORLSATT).
   RUN whandle_UI (INPUT-OUTPUT ordningnr, BTN_BORTKAB:HANDLE IN FRAME FRAME-KABLAR).
   RUN whandle_UI (INPUT-OUTPUT ordningnr, BTN_AVSLUTA:HANDLE IN FRAME DEFAULT-FRAME).
   RUN whandle_UI (INPUT-OUTPUT ordningnr, tempkabel.DIAMETER:HANDLE IN BROWSE BRW_KAB).
   RUN whandle_UI (INPUT-OUTPUT ordningnr, BTN_MORDUPP:HANDLE IN FRAME FRAME-FORLSATT).
   RUN whandle_UI (INPUT-OUTPUT ordningnr, BTN_MORDNER:HANDLE IN FRAME FRAME-FORLSATT).
   RUN whandle_UI (INPUT-OUTPUT ordningnr, tempkkopp.ANTAL:HANDLE IN BROWSE BRW_KKOPP).
   RUN SCHAKTADMINN.P PERSISTENT SET schakth (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT TABLE whandltemp).                                         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

