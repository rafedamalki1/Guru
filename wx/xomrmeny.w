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

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{LDALIAS8.I}
&SCOPED-DEFINE NEW NEW
{OMRTEMPW.I}
{GLOBVAR2DEL1.I}
DEFINE {&NEW} SHARED TEMP-TABLE omrtemp2 NO-UNDO LIKE omrtemp.
&Scoped-define LEFT-BROWSE 1
&Scoped-define RIGHT-BROWSE 2
&Scoped-define ARROWS 3

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_UROMR

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES omrtemp omrtemp2

/* Definitions for BROWSE BRW_UROMR                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_UROMR omrtemp.OMRADE omrtemp.ANVANDARE ~
omrtemp.NAMN omrtemp.PANDRA 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UROMR omrtemp.OMRADE 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_UROMR omrtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_UROMR omrtemp
&Scoped-define QUERY-STRING-BRW_UROMR FOR EACH omrtemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_UROMR OPEN QUERY BRW_UROMR FOR EACH omrtemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_UROMR omrtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UROMR omrtemp


/* Definitions for BROWSE BRW_VOMR                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_VOMR omrtemp2.OMRADE omrtemp2.ANVANDARE ~
omrtemp2.NAMN omrtemp2.PANDRA 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VOMR omrtemp2.OMRADE 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_VOMR omrtemp2
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_VOMR omrtemp2
&Scoped-define QUERY-STRING-BRW_VOMR FOR EACH omrtemp2 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_VOMR OPEN QUERY BRW_VOMR FOR EACH omrtemp2 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_VOMR omrtemp2
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VOMR omrtemp2


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BRW_UROMR}~
    ~{&OPEN-QUERY-BRW_VOMR}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS COMBO-BOX-1 FILL-IN-2 BRW_UROMR BRW_VOMR ~
BTN_ALLOVER BTN_OVER BTN_BACK BTN_ALLBACK BUTTON-13 BTN_NY BTN_BORT ~
BTN_ANDRA BTN_VIS BTN_AVB RECT-23 RECT-26 
&Scoped-Define DISPLAYED-OBJECTS COMBO-BOX-1 FILL-IN-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_ALLBACK 
     IMAGE-UP FILE "BILDER\rewind-u":U
     LABEL "Alla aonr i listan":L 
     SIZE 5.5 BY 1.75 TOOLTIP "Alla valda aonr tas bort från vallistan"
     FONT 11.

DEFINE BUTTON BTN_ALLOVER 
     IMAGE-UP FILE "BILDER\forwrd-u":U
     LABEL "Alla aonr i listan":L 
     SIZE 5.5 BY 1.75 TOOLTIP "Alla aonr väljs"
     FONT 11.

DEFINE BUTTON BTN_ANDRA 
     LABEL "Ändra område":L 
     SIZE 16 BY 1.5.

DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 16 BY 1.5.

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 5.5 BY 1.75 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_BORT  NO-CONVERT-3D-COLORS
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NY  NO-CONVERT-3D-COLORS
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 5.5 BY 1.75 TOOLTIP "Markerade väljs".

DEFINE BUTTON BTN_VIS 
     LABEL "Visa":L 
     SIZE 16 BY 1.5.

DEFINE BUTTON BUTTON-13 
     LABEL "Urvalsresultat" 
     SIZE 16.5 BY 1.

DEFINE VARIABLE COMBO-BOX-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Sök område" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 16 BY .96 NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(256)":U 
     LABEL "Avdelning" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 102.5 BY 21.75.

DEFINE RECTANGLE RECT-26
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 101.5 BY 2.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_UROMR FOR 
      omrtemp SCROLLING.

DEFINE QUERY BRW_VOMR FOR 
      omrtemp2 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_UROMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UROMR C-Win _STRUCTURED
  QUERY BRW_UROMR NO-LOCK DISPLAY
      omrtemp.OMRADE COLUMN-LABEL "Område" FORMAT "X(6)":U
      omrtemp.ANVANDARE COLUMN-LABEL "Användare" FORMAT "X(12)":U
      omrtemp.NAMN COLUMN-LABEL "Namn" FORMAT "X(20)":U WIDTH 18
      omrtemp.PANDRA FORMAT "Ja/Nej":U
  ENABLE
      omrtemp.OMRADE
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS MULTIPLE SIZE 46.5 BY 14.75
         TITLE "Urvalsresultat" ROW-HEIGHT-CHARS .42.

DEFINE BROWSE BRW_VOMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VOMR C-Win _STRUCTURED
  QUERY BRW_VOMR NO-LOCK DISPLAY
      omrtemp2.OMRADE COLUMN-LABEL "Område" FORMAT "X(6)":U
      omrtemp2.ANVANDARE COLUMN-LABEL "Användare" FORMAT "X(12)":U
      omrtemp2.NAMN COLUMN-LABEL "Namn" FORMAT "X(20)":U
      omrtemp2.PANDRA FORMAT "Ja/Nej":U
  ENABLE
      omrtemp2.OMRADE
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS MULTIPLE SIZE 48.38 BY 14.75
         TITLE "Arbeta vidare med".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     COMBO-BOX-1 AT ROW 1.63 COL 19 COLON-ALIGNED
     FILL-IN-2 AT ROW 2.63 COL 19 COLON-ALIGNED
     BRW_UROMR AT ROW 4.25 COL 2
     BRW_VOMR AT ROW 4.25 COL 54.25
     BTN_ALLOVER AT ROW 8.04 COL 48.63
     BTN_OVER AT ROW 10.21 COL 48.63
     BTN_BACK AT ROW 12.42 COL 48.63
     BTN_ALLBACK AT ROW 14.63 COL 48.63
     BUTTON-13 AT ROW 19 COL 2
     BTN_NY AT ROW 19 COL 54.38
     BTN_BORT AT ROW 19 COL 66.38
     BTN_ANDRA AT ROW 20.38 COL 2.25
     BTN_VIS AT ROW 20.38 COL 18.13
     BTN_AVB AT ROW 20.38 COL 34.13
     RECT-23 AT ROW 1 COL 1
     RECT-26 AT ROW 20.13 COL 1.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 102.63 BY 21.79.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: omrtemp T "?" NO-UNDO temp-db omrtemp
      TABLE: omrtemp2 T "?" NO-UNDO temp-db omrtemp2
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Område"
         HEIGHT             = 21.92
         WIDTH              = 102.88
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
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BRW_UROMR FILL-IN-2 DEFAULT-FRAME */
/* BROWSE-TAB BRW_VOMR BRW_UROMR DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UROMR
/* Query rebuild information for BROWSE BRW_UROMR
     _TblList          = "Temp-Tables.omrtemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.omrtemp.OMRADE
"omrtemp.OMRADE" "Område" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.omrtemp.ANVANDARE
"omrtemp.ANVANDARE" "Användare" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.omrtemp.NAMN
"omrtemp.NAMN" "Namn" ? "character" ? ? ? ? ? ? no ? no no "18" yes no no "U" "" ""
     _FldNameList[4]   = Temp-Tables.omrtemp.PANDRA
     _Query            is OPENED
*/  /* BROWSE BRW_UROMR */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VOMR
/* Query rebuild information for BROWSE BRW_VOMR
     _TblList          = "Temp-Tables.omrtemp2"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.omrtemp2.OMRADE
"omrtemp2.OMRADE" "Område" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.omrtemp2.ANVANDARE
"omrtemp2.ANVANDARE" "Användare" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.omrtemp2.NAMN
"omrtemp2.NAMN" "Namn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   = Temp-Tables.omrtemp2.PANDRA
     _Query            is OPENED
*/  /* BROWSE BRW_VOMR */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Område */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Område */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_UROMR
&Scoped-define SELF-NAME BRW_UROMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_UROMR C-Win
ON ANY-PRINTABLE OF BRW_UROMR IN FRAME DEFAULT-FRAME /* Urvalsresultat */
DO:
   RUN hittadyn_UI IN brwproc[{&LEFT-BROWSE}].  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_UROMR C-Win
ON START-SEARCH OF BRW_UROMR IN FRAME DEFAULT-FRAME /* Urvalsresultat */
DO:
   RUN colsortdynbrw_UI IN brwproc[{&LEFT-BROWSE}] (INPUT "").
   RUN lastselectdyn_UI IN brwproc[{&LEFT-BROWSE}].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_UROMR C-Win
ON VALUE-CHANGED OF BRW_UROMR IN FRAME DEFAULT-FRAME /* Urvalsresultat */
DO:
   RUN fetchselrowid_UI IN brwproc[{&LEFT-BROWSE}].  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VOMR
&Scoped-define SELF-NAME BRW_VOMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VOMR C-Win
ON ANY-PRINTABLE OF BRW_VOMR IN FRAME DEFAULT-FRAME /* Arbeta vidare med */
DO:
   RUN hittadyn_UI IN brwproc[{&RIGHT-BROWSE}].  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VOMR C-Win
ON START-SEARCH OF BRW_VOMR IN FRAME DEFAULT-FRAME /* Arbeta vidare med */
DO:
   RUN colsortdynbrw_UI IN brwproc[{&RIGHT-BROWSE}] (INPUT "").
   RUN lastselectdyn_UI IN brwproc[{&RIGHT-BROWSE}].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VOMR C-Win
ON VALUE-CHANGED OF BRW_VOMR IN FRAME DEFAULT-FRAME /* Arbeta vidare med */
DO:
   RUN fetchselrowid_UI IN brwproc[{&RIGHT-BROWSE}].  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ALLBACK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ALLBACK C-Win
ON CHOOSE OF BTN_ALLBACK IN FRAME DEFAULT-FRAME /* Alla aonr i listan */
DO:
   RUN all_back_UI IN brwproc[{&ARROWS}].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_ALLOVER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_ALLOVER C-Win
ON CHOOSE OF BTN_ALLOVER IN FRAME DEFAULT-FRAME /* Alla aonr i listan */
DO:
   RUN all_over_UI IN brwproc[{&ARROWS}].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BACK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BACK C-Win
ON CHOOSE OF BTN_BACK IN FRAME DEFAULT-FRAME
DO:
   RUN sel_back_UI IN brwproc[{&ARROWS}].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OVER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OVER C-Win
ON CHOOSE OF BTN_OVER IN FRAME DEFAULT-FRAME
DO:
   RUN sel_over_UI IN brwproc[{&ARROWS}].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_UROMR
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
   {OMRHMT.I}.
   RUN allstartbrw_UI.  
   RUN enable_UI.
   RUN openbdyn_UI IN brwproc[{&LEFT-BROWSE}] (INPUT "").
   {musarrow.i}
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
      WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI C-Win 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   ASSIGN
   omrtemp.OMRADE:READ-ONLY IN BROWSE BRW_UROMR = TRUE    
   omrtemp2.OMRADE:READ-ONLY IN BROWSE  BRW_VOMR = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[{&LEFT-BROWSE}] (INPUT BRW_UROMR:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[{&RIGHT-BROWSE}] (INPUT BRW_VOMR:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNARROW.P PERSISTENT SET brwproc[{&ARROWS}] (INPUT BRW_UROMR:HANDLE, INPUT BRW_VOMR:HANDLE).
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
  DISPLAY COMBO-BOX-1 FILL-IN-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE COMBO-BOX-1 FILL-IN-2 BRW_UROMR BRW_VOMR BTN_ALLOVER BTN_OVER BTN_BACK 
         BTN_ALLBACK BUTTON-13 BTN_NY BTN_BORT BTN_ANDRA BTN_VIS BTN_AVB 
         RECT-23 RECT-26 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

