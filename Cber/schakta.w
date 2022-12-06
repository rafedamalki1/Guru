&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


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


{ALLDEF.I}
{WHANDLTEMP.I}
&Scoped-define NEW 
  
{GLOBVAR2DEL1.I}



DEFINE VARIABLE schakth AS HANDLE NO-UNDO.

&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{KONSTRMTRL.I}
{SCADMIN.I}
/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_AKAB

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES akltemp tempforlagg mkltemp okltemp pkltemp ~
hdpunkttemp tempsamforlagg samval hdschakttemp hdschaktfortemp tempytbelagg

/* Definitions for BROWSE BRW_AKAB                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_AKAB akltemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_AKAB 
&Scoped-define QUERY-STRING-BRW_AKAB FOR EACH akltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_AKAB OPEN QUERY BRW_AKAB FOR EACH akltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_AKAB akltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_AKAB akltemp


/* Definitions for BROWSE BRW_FOR                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_FOR tempforlagg.BENAMNING ~
tempforlagg.DJUP tempforlagg.FAKTOR tempforlagg.FORLAGG tempforlagg.MARK ~
tempforlagg.ROR tempforlagg.SAM tempforlagg.TILLAGG 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_FOR 
&Scoped-define QUERY-STRING-BRW_FOR FOR EACH tempforlagg NO-LOCK
&Scoped-define OPEN-QUERY-BRW_FOR OPEN QUERY BRW_FOR FOR EACH tempforlagg NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_FOR tempforlagg
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_FOR tempforlagg


/* Definitions for BROWSE BRW_MKAB                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_MKAB mkltemp.PID 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MKAB 
&Scoped-define QUERY-STRING-BRW_MKAB FOR EACH mkltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MKAB OPEN QUERY BRW_MKAB FOR EACH mkltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MKAB mkltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MKAB mkltemp


/* Definitions for BROWSE BRW_OKAB                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_OKAB okltemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_OKAB 
&Scoped-define QUERY-STRING-BRW_OKAB FOR EACH okltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_OKAB OPEN QUERY BRW_OKAB FOR EACH okltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_OKAB okltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_OKAB okltemp


/* Definitions for BROWSE BRW_PKAB                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_PKAB pkltemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PKAB 
&Scoped-define QUERY-STRING-BRW_PKAB FOR EACH pkltemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_PKAB OPEN QUERY BRW_PKAB FOR EACH pkltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_PKAB pkltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PKAB pkltemp


/* Definitions for BROWSE BRW_PUNKT                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_PUNKT hdpunkttemp.PID ~
hdpunkttemp.ORDNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PUNKT 
&Scoped-define QUERY-STRING-BRW_PUNKT FOR EACH hdpunkttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_PUNKT OPEN QUERY BRW_PUNKT FOR EACH hdpunkttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_PUNKT hdpunkttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PUNKT hdpunkttemp


/* Definitions for BROWSE BRW_SAM                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_SAM tempsamforlagg.BENAMNING ~
tempsamforlagg.KOD tempsamforlagg.DIAMETER 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_SAM 
&Scoped-define QUERY-STRING-BRW_SAM FOR EACH tempsamforlagg NO-LOCK
&Scoped-define OPEN-QUERY-BRW_SAM OPEN QUERY BRW_SAM FOR EACH tempsamforlagg NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_SAM tempsamforlagg
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_SAM tempsamforlagg


/* Definitions for BROWSE BRW_SAMVAL                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_SAMVAL samval.BENAMNING samval.ANTAL 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_SAMVAL 
&Scoped-define QUERY-STRING-BRW_SAMVAL FOR EACH samval NO-LOCK
&Scoped-define OPEN-QUERY-BRW_SAMVAL OPEN QUERY BRW_SAMVAL FOR EACH samval NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_SAMVAL samval
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_SAMVAL samval


/* Definitions for BROWSE BRW_SCHAKT                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_SCHAKT hdschakttemp.SID ~
hdschakttemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_SCHAKT 
&Scoped-define QUERY-STRING-BRW_SCHAKT FOR EACH hdschakttemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_SCHAKT OPEN QUERY BRW_SCHAKT FOR EACH hdschakttemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_SCHAKT hdschakttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_SCHAKT hdschakttemp


/* Definitions for BROWSE BRW_SF                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_SF hdschaktfortemp.FID ~
hdschaktfortemp.PID1 hdschaktfortemp.PID2 hdschaktfortemp.LAGID ~
hdschaktfortemp.BREDD hdschaktfortemp.DJUP hdschaktfortemp.LANGD ~
hdschaktfortemp.YTBELAGG 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_SF 
&Scoped-define QUERY-STRING-BRW_SF FOR EACH hdschaktfortemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_SF OPEN QUERY BRW_SF FOR EACH hdschaktfortemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_SF hdschaktfortemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_SF hdschaktfortemp


/* Definitions for BROWSE BRW_YT                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_YT tempytbelagg.YTBELAGG 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_YT 
&Scoped-define QUERY-STRING-BRW_YT FOR EACH tempytbelagg NO-LOCK
&Scoped-define OPEN-QUERY-BRW_YT OPEN QUERY BRW_YT FOR EACH tempytbelagg NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_YT tempytbelagg
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_YT tempytbelagg


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BRW_AKAB}~
    ~{&OPEN-QUERY-BRW_FOR}~
    ~{&OPEN-QUERY-BRW_MKAB}~
    ~{&OPEN-QUERY-BRW_OKAB}~
    ~{&OPEN-QUERY-BRW_PKAB}~
    ~{&OPEN-QUERY-BRW_PUNKT}~
    ~{&OPEN-QUERY-BRW_SAM}~
    ~{&OPEN-QUERY-BRW_SAMVAL}~
    ~{&OPEN-QUERY-BRW_SCHAKT}~
    ~{&OPEN-QUERY-BRW_SF}~
    ~{&OPEN-QUERY-BRW_YT}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_PUNKT BRW_SCHAKT RECT-1 RECT-2 ~
TOGGLE-FOR BRW_FOR CMB_KAB TOGGLE-SAM BRW_SAM BRW_SAMVAL BTN_ADDSAM ~
BTN_KABFRSC BTN_KABTILLSC BTN_REMSAM BRW_OKAB BRW_PKAB BTN_KABTILLP ~
TOGGLE-YT BRW_YT BTN_KABFRP BTN_KOPP BRW_MKAB BRW_SF BRW_AKAB 
&Scoped-Define DISPLAYED-OBJECTS TOGGLE-FOR CMB_KAB TOGGLE-SAM TOGGLE-YT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ADDSAM 
     LABEL ">" 
     SIZE 4 BY 1.25.

DEFINE BUTTON BTN_KABFRP 
     LABEL "<" 
     SIZE 4.5 BY 1.5.

DEFINE BUTTON BTN_KABFRSC 
     LABEL "^" 
     SIZE 4.5 BY 1.25.

DEFINE BUTTON BTN_KABTILLP 
     LABEL ">" 
     SIZE 4.5 BY 1.5.

DEFINE BUTTON BTN_KABTILLSC 
     LABEL "V" 
     SIZE 4.5 BY 1.25.

DEFINE BUTTON BTN_KOPP 
     LABEL "Koppla" 
     SIZE 11.5 BY 1.75.

DEFINE BUTTON BTN_REMSAM 
     LABEL "<" 
     SIZE 4 BY 1.25.

DEFINE VARIABLE CMB_KAB LIKE tempkabel.KABEL
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 39.5 BY .25.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE .5 BY 10.

DEFINE VARIABLE TOGGLE-FOR AS LOGICAL INITIAL no 
     LABEL "Toggle 1" 
     VIEW-AS TOGGLE-BOX
     SIZE 2 BY 1 NO-UNDO.

DEFINE VARIABLE TOGGLE-SAM AS LOGICAL INITIAL no 
     LABEL "Toggle 2" 
     VIEW-AS TOGGLE-BOX
     SIZE 2.5 BY 1.25 NO-UNDO.

DEFINE VARIABLE TOGGLE-YT AS LOGICAL INITIAL no 
     LABEL "Toggle 3" 
     VIEW-AS TOGGLE-BOX
     SIZE 2.5 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_AKAB FOR 
      akltemp SCROLLING.

DEFINE QUERY BRW_FOR FOR 
      tempforlagg SCROLLING.

DEFINE QUERY BRW_MKAB FOR 
      mkltemp SCROLLING.

DEFINE QUERY BRW_OKAB FOR 
      okltemp SCROLLING.

DEFINE QUERY BRW_PKAB FOR 
      pkltemp SCROLLING.

DEFINE QUERY BRW_PUNKT FOR 
      hdpunkttemp SCROLLING.

DEFINE QUERY BRW_SAM FOR 
      tempsamforlagg SCROLLING.

DEFINE QUERY BRW_SAMVAL FOR 
      samval SCROLLING.

DEFINE QUERY BRW_SCHAKT FOR 
      hdschakttemp SCROLLING.

DEFINE QUERY BRW_SF FOR 
      hdschaktfortemp SCROLLING.

DEFINE QUERY BRW_YT FOR 
      tempytbelagg SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_AKAB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_AKAB C-Win _STRUCTURED
  QUERY BRW_AKAB NO-LOCK DISPLAY
      akltemp.BENAMNING FORMAT "X(8)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 18.5 BY 4 ROW-HEIGHT-CHARS .71 EXPANDABLE.

DEFINE BROWSE BRW_FOR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_FOR C-Win _STRUCTURED
  QUERY BRW_FOR NO-LOCK DISPLAY
      tempforlagg.BENAMNING FORMAT "X(8)":U WIDTH 10
      tempforlagg.DJUP FORMAT "->>>>>>9":U WIDTH 6.5
      tempforlagg.FAKTOR FORMAT "->>>>9.99":U WIDTH 7.5
      tempforlagg.FORLAGG FORMAT "X(8)":U
      tempforlagg.MARK FORMAT "yes/no":U
      tempforlagg.ROR FORMAT "yes/no":U
      tempforlagg.SAM FORMAT "yes/no":U
      tempforlagg.TILLAGG FORMAT "->>>>>>9":U WIDTH 6.25
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 54.63 BY 4.5 EXPANDABLE.

DEFINE BROWSE BRW_MKAB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MKAB C-Win _STRUCTURED
  QUERY BRW_MKAB NO-LOCK DISPLAY
      mkltemp.PID FORMAT "->>>>>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 12.63 BY 9 EXPANDABLE.

DEFINE BROWSE BRW_OKAB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_OKAB C-Win _STRUCTURED
  QUERY BRW_OKAB NO-LOCK DISPLAY
      okltemp.BENAMNING FORMAT "X(8)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 14.75 BY 4.46 EXPANDABLE.

DEFINE BROWSE BRW_PKAB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PKAB C-Win _STRUCTURED
  QUERY BRW_PKAB NO-LOCK DISPLAY
      pkltemp.BENAMNING FORMAT "X(8)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 15.5 BY 4.5 EXPANDABLE.

DEFINE BROWSE BRW_PUNKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PUNKT C-Win _STRUCTURED
  QUERY BRW_PUNKT NO-LOCK DISPLAY
      hdpunkttemp.PID FORMAT "->>>>>>9":U
      hdpunkttemp.ORDNING FORMAT "->>>>>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 26.5 BY 6.25 ROW-HEIGHT-CHARS .54 EXPANDABLE.

DEFINE BROWSE BRW_SAM
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_SAM C-Win _STRUCTURED
  QUERY BRW_SAM NO-LOCK DISPLAY
      tempsamforlagg.BENAMNING FORMAT "X(8)":U
      tempsamforlagg.KOD FORMAT "X(8)":U WIDTH 6
      tempsamforlagg.DIAMETER FORMAT "->>>>>>9":U WIDTH 9
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 27 BY 4 EXPANDABLE.

DEFINE BROWSE BRW_SAMVAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_SAMVAL C-Win _STRUCTURED
  QUERY BRW_SAMVAL NO-LOCK DISPLAY
      samval.BENAMNING FORMAT "x(40)":U WIDTH 9.5
      samval.ANTAL FORMAT "->>>>>>9":U WIDTH 3.63
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 18 BY 3.83 EXPANDABLE.

DEFINE BROWSE BRW_SCHAKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_SCHAKT C-Win _STRUCTURED
  QUERY BRW_SCHAKT NO-LOCK DISPLAY
      hdschakttemp.SID FORMAT "->>>>>>9":U WIDTH 4
      hdschakttemp.BENAMNING FORMAT "X(8)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 22 BY 3.25 EXPANDABLE.

DEFINE BROWSE BRW_SF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_SF C-Win _STRUCTURED
  QUERY BRW_SF NO-LOCK DISPLAY
      hdschaktfortemp.FID FORMAT "->>>>>>9":U WIDTH 3.63
      hdschaktfortemp.PID1 COLUMN-LABEL "Från" FORMAT "->>>>>>9":U
            WIDTH 5
      hdschaktfortemp.PID2 COLUMN-LABEL "Till" FORMAT "->>>>>>9":U
            WIDTH 5.5
      hdschaktfortemp.LAGID COLUMN-LABEL "Sätt" FORMAT "X(8)":U
            WIDTH 10.5
      hdschaktfortemp.BREDD COLUMN-LABEL "Bredd" FORMAT "->>>>>>9":U
            WIDTH 6.5
      hdschaktfortemp.DJUP COLUMN-LABEL "Djup" FORMAT "->>>>>>9":U
            WIDTH 7
      hdschaktfortemp.LANGD COLUMN-LABEL "Längd" FORMAT "->>>>9.99":U
            WIDTH 7.5
      hdschaktfortemp.YTBELAGG COLUMN-LABEL "Ytbeläggning" FORMAT "X(8)":U
            WIDTH 12.5
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 64.5 BY 6.75 ROW-HEIGHT-CHARS .63 EXPANDABLE.

DEFINE BROWSE BRW_YT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_YT C-Win _STRUCTURED
  QUERY BRW_YT NO-LOCK DISPLAY
      tempytbelagg.YTBELAGG FORMAT "X(8)":U WIDTH 13.13
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 16 BY 3.25 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BRW_PUNKT AT ROW 1 COL 1
     BRW_SCHAKT AT ROW 1 COL 29
     TOGGLE-FOR AT ROW 1.75 COL 110
     BRW_FOR AT ROW 1.88 COL 54.5
     CMB_KAB AT ROW 6.25 COL 26.5 COLON-ALIGNED NO-LABEL
     TOGGLE-SAM AT ROW 6.75 COL 109.5
     BRW_SAM AT ROW 6.92 COL 54.5
     BRW_SAMVAL AT ROW 7.04 COL 90.75
     BTN_ADDSAM AT ROW 7.5 COL 83.5
     BTN_KABFRSC AT ROW 8.25 COL 27.5
     BTN_KABTILLSC AT ROW 8.25 COL 34
     BTN_REMSAM AT ROW 9.63 COL 83.63
     BRW_OKAB AT ROW 10.5 COL 2.25
     BRW_PKAB AT ROW 10.5 COL 24.13
     BTN_KABTILLP AT ROW 11 COL 18.5
     TOGGLE-YT AT ROW 11.5 COL 110
     BRW_YT AT ROW 11.71 COL 92.88
     BTN_KABFRP AT ROW 12.75 COL 18.5
     BTN_KOPP AT ROW 14.5 COL 68
     BRW_MKAB AT ROW 16.25 COL 22.5
     BRW_SF AT ROW 17.08 COL 46.88
     BRW_AKAB AT ROW 21 COL 1.5
     RECT-1 AT ROW 15.5 COL 1.5
     RECT-2 AT ROW 15.75 COL 40.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112.13 BY 24.96.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: akltemp T "?" NO-UNDO temp-db akltemp
      TABLE: hdforlkabtemp T "?" NO-UNDO temp-db hdforlkabtemp
      TABLE: hdforlsamtemp T "?" NO-UNDO temp-db hdforlsamtemp
      TABLE: hdkabellinjetemp T "?" NO-UNDO temp-db hdkabellinjetemp
      TABLE: hdpunkttemp T "?" NO-UNDO temp-db hdpunkttemp
      TABLE: hdschaktfortemp T "?" NO-UNDO temp-db hdschaktfortemp
      TABLE: hdschakttemp T "?" NO-UNDO temp-db hdschakttemp
      TABLE: mkltemp T "?" NO-UNDO temp-db mkltemp
      TABLE: okltemp T "?" NO-UNDO temp-db okltemp
      TABLE: pkltemp T "?" NO-UNDO temp-db pkltemp
      TABLE: samval T "?" NO-UNDO temp-db samval
      TABLE: tempforlagg T "?" NO-UNDO temp-db tempforlagg
      TABLE: tempkabel T "?" NO-UNDO temp-db tempkabel
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
         HEIGHT             = 24.96
         WIDTH              = 112.13
         MAX-HEIGHT         = 24.96
         MAX-WIDTH          = 112.25
         VIRTUAL-HEIGHT     = 24.96
         VIRTUAL-WIDTH      = 112.25
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
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BRW_PUNKT 1 DEFAULT-FRAME */
/* BROWSE-TAB BRW_SCHAKT BRW_PUNKT DEFAULT-FRAME */
/* BROWSE-TAB BRW_FOR TOGGLE-FOR DEFAULT-FRAME */
/* BROWSE-TAB BRW_SAM TOGGLE-SAM DEFAULT-FRAME */
/* BROWSE-TAB BRW_SAMVAL BRW_SAM DEFAULT-FRAME */
/* BROWSE-TAB BRW_OKAB BTN_REMSAM DEFAULT-FRAME */
/* BROWSE-TAB BRW_PKAB BRW_OKAB DEFAULT-FRAME */
/* BROWSE-TAB BRW_YT TOGGLE-YT DEFAULT-FRAME */
/* BROWSE-TAB BRW_MKAB BTN_KOPP DEFAULT-FRAME */
/* BROWSE-TAB BRW_SF BRW_MKAB DEFAULT-FRAME */
/* BROWSE-TAB BRW_AKAB BRW_SF DEFAULT-FRAME */
/* SETTINGS FOR COMBO-BOX CMB_KAB IN FRAME DEFAULT-FRAME
   LIKE = Temp-Tables.tempkabel.KABEL EXP-SIZE                          */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_AKAB
/* Query rebuild information for BROWSE BRW_AKAB
     _TblList          = "Temp-Tables.akltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.akltemp.BENAMNING
     _Query            is OPENED
*/  /* BROWSE BRW_AKAB */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_FOR
/* Query rebuild information for BROWSE BRW_FOR
     _TblList          = "Temp-Tables.tempforlagg"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.tempforlagg.BENAMNING
"BENAMNING" ? ? "character" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.tempforlagg.DJUP
"DJUP" ? ? "integer" ? ? ? ? ? ? no ? no no "6.5" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.tempforlagg.FAKTOR
"FAKTOR" ? ? "decimal" ? ? ? ? ? ? no ? no no "7.5" yes no no "U" "" ""
     _FldNameList[4]   = Temp-Tables.tempforlagg.FORLAGG
     _FldNameList[5]   = Temp-Tables.tempforlagg.MARK
     _FldNameList[6]   = Temp-Tables.tempforlagg.ROR
     _FldNameList[7]   = Temp-Tables.tempforlagg.SAM
     _FldNameList[8]   > Temp-Tables.tempforlagg.TILLAGG
"TILLAGG" ? ? "integer" ? ? ? ? ? ? no ? no no "6.25" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_FOR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MKAB
/* Query rebuild information for BROWSE BRW_MKAB
     _TblList          = "Temp-Tables.mkltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.mkltemp.PID
     _Query            is OPENED
*/  /* BROWSE BRW_MKAB */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_OKAB
/* Query rebuild information for BROWSE BRW_OKAB
     _TblList          = "Temp-Tables.okltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.okltemp.BENAMNING
     _Query            is OPENED
*/  /* BROWSE BRW_OKAB */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PKAB
/* Query rebuild information for BROWSE BRW_PKAB
     _TblList          = "Temp-Tables.pkltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.pkltemp.BENAMNING
     _Query            is OPENED
*/  /* BROWSE BRW_PKAB */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PUNKT
/* Query rebuild information for BROWSE BRW_PUNKT
     _TblList          = "Temp-Tables.hdpunkttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.hdpunkttemp.PID
     _FldNameList[2]   = Temp-Tables.hdpunkttemp.ORDNING
     _Query            is OPENED
*/  /* BROWSE BRW_PUNKT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_SAM
/* Query rebuild information for BROWSE BRW_SAM
     _TblList          = "Temp-Tables.tempsamforlagg"
     _Options          = "NO-LOCK"
     _FldNameList[1]   = Temp-Tables.tempsamforlagg.BENAMNING
     _FldNameList[2]   > Temp-Tables.tempsamforlagg.KOD
"KOD" ? ? "character" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.tempsamforlagg.DIAMETER
"DIAMETER" ? ? "integer" ? ? ? ? ? ? no ? no no "8.13" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_SAM */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_SAMVAL
/* Query rebuild information for BROWSE BRW_SAMVAL
     _TblList          = "Temp-Tables.samval"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.samval.BENAMNING
"BENAMNING" ? ? "character" ? ? ? ? ? ? no ? no no "9.5" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.samval.ANTAL
"ANTAL" ? ? "integer" ? ? ? ? ? ? no ? no no "5.13" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_SAMVAL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_SCHAKT
/* Query rebuild information for BROWSE BRW_SCHAKT
     _TblList          = "Temp-Tables.hdschakttemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.hdschakttemp.SID
"hdschakttemp.SID" ? ? "integer" ? ? ? ? ? ? no ? no no "4" yes no no "U" "" ""
     _FldNameList[2]   = Temp-Tables.hdschakttemp.BENAMNING
     _Query            is OPENED
*/  /* BROWSE BRW_SCHAKT */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_SF
/* Query rebuild information for BROWSE BRW_SF
     _TblList          = "Temp-Tables.hdschaktfortemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.hdschaktfortemp.FID
"FID" ? ? "integer" ? ? ? ? ? ? no ? no no "3.63" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.hdschaktfortemp.PID1
"PID1" "Från" ? "integer" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.hdschaktfortemp.PID2
"PID2" "Till" ? "integer" ? ? ? ? ? ? no ? no no "5.5" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.hdschaktfortemp.LAGID
"LAGID" "Sätt" ? "character" ? ? ? ? ? ? no ? no no "10.5" yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.hdschaktfortemp.BREDD
"BREDD" "Bredd" ? "integer" ? ? ? ? ? ? no ? no no "6.5" yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.hdschaktfortemp.DJUP
"DJUP" "Djup" ? "integer" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.hdschaktfortemp.LANGD
"LANGD" "Längd" ? "decimal" ? ? ? ? ? ? no ? no no "7.5" yes no no "U" "" ""
     _FldNameList[8]   > Temp-Tables.hdschaktfortemp.YTBELAGG
"YTBELAGG" "Ytbeläggning" ? "character" ? ? ? ? ? ? no ? no no "12.5" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_SF */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_YT
/* Query rebuild information for BROWSE BRW_YT
     _TblList          = "Temp-Tables.tempytbelagg"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.tempytbelagg.YTBELAGG
"YTBELAGG" ? ? "character" ? ? ? ? ? ? no ? no no "13.13" yes no no "U" "" ""
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


&Scoped-define BROWSE-NAME BRW_AKAB
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  {WIN_M_START.I}
  {muswait.i}
   
   
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
  DISPLAY TOGGLE-FOR CMB_KAB TOGGLE-SAM TOGGLE-YT 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BRW_PUNKT BRW_SCHAKT RECT-1 RECT-2 TOGGLE-FOR BRW_FOR CMB_KAB 
         TOGGLE-SAM BRW_SAM BRW_SAMVAL BTN_ADDSAM BTN_KABFRSC BTN_KABTILLSC 
         BTN_REMSAM BRW_OKAB BRW_PKAB BTN_KABTILLP TOGGLE-YT BRW_YT BTN_KABFRP 
         BTN_KOPP BRW_MKAB BRW_SF BRW_AKAB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
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
   DEFINE INPUT PARAMETER ordnr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER ordh AS HANDLE NO-UNDO.
   ASSIGN
   whandltemp.WF[ordnr] = ordh.
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
   ordningnr = 0.   
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,C-WIN:HANDLE).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BRW_PUNKT:HANDLE IN FRAME DEFAULT-FRAME).
   ordningnr = ordningnr + 1.  
   RUN whandle_UI (INPUT ordningnr,BRW_OKAB:HANDLE IN FRAME DEFAULT-FRAME).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BRW_PKAB:HANDLE IN FRAME DEFAULT-FRAME).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BRW_MKAB:HANDLE IN FRAME DEFAULT-FRAME).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BRW_AKAB:HANDLE IN FRAME DEFAULT-FRAME).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,CMB_KAB:HANDLE IN FRAME DEFAULT-FRAME).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BRW_SCHAKT:HANDLE IN FRAME DEFAULT-FRAME).
   ordningnr = ordningnr + 1.
   RUN whandle_UI (INPUT ordningnr,BTN_KABTILLSC:HANDLE IN FRAME DEFAULT-FRAME).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_KABFRSC:HANDLE IN FRAME DEFAULT-FRAME).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_KABTILLP:HANDLE IN FRAME DEFAULT-FRAME).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BTN_KABFRP:HANDLE IN FRAME DEFAULT-FRAME).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BRW_FOR:HANDLE IN FRAME DEFAULT-FRAME). /*ny*/
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BRW_SAM:HANDLE IN FRAME DEFAULT-FRAME).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BRW_YT:HANDLE IN FRAME DEFAULT-FRAME).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BRW_SAMVAL:HANDLE IN FRAME DEFAULT-FRAME).
   ordningnr = ordningnr + 1.      
   RUN whandle_UI (INPUT ordningnr,BRW_SF:HANDLE IN FRAME DEFAULT-FRAME).
   
   RUN SCHAKTIT.P PERSISTENT SET schakth (INPUT THIS-PROCEDURE ,INPUT framesizeh,INPUT TABLE whandltemp).                                         
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

