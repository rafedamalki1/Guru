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
DEFINE INPUT PARAMETER aonrvar AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER delnrvar AS INTEGER NO-UNDO.
{ALLDEF.I}

/* Local Variable Definitions ---                                       */
&Scoped-define SHARED  
{ANVTEMPS.I}
{GLOBVAR2DEL1.I}
DEFINE SHARED TEMP-TABLE omranvandartemp NO-UNDO LIKE anvandartemp.
DEFINE SHARED VARIABLE anvapph AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_ANV

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES anvandartemp omranvandartemp

/* Definitions for BROWSE BRW_ANV                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_ANV anvandartemp.ANVANDARE ~
anvandartemp.AV-NAMN anvandartemp.PERSONALKOD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ANV 
&Scoped-define QUERY-STRING-BRW_ANV FOR EACH anvandartemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ANV OPEN QUERY BRW_ANV FOR EACH anvandartemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ANV anvandartemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ANV anvandartemp


/* Definitions for BROWSE BRW_ANVOMR                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_ANVOMR omranvandartemp.ANVANDARE ~
omranvandartemp.AV-NAMN omranvandartemp.PERSONALKOD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ANVOMR 
&Scoped-define QUERY-STRING-BRW_ANVOMR FOR EACH omranvandartemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_ANVOMR OPEN QUERY BRW_ANVOMR FOR EACH omranvandartemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ANVOMR omranvandartemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ANVOMR omranvandartemp


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_ANV BRW_ANVOMR BTN_ALLOVER BTN_OVER ~
BTN_BACK BTN_ALLBACK BTN_OK BTN_AVBRYT 

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
     SIZE 4 BY 1.21 TOOLTIP "Alla valda plannr tas bort från vallistan"
     FONT 11.

DEFINE BUTTON BTN_ALLOVER 
     IMAGE-UP FILE "BILDER\forwrd-u":U
     LABEL "Alla plannr i listan":L 
     SIZE 4 BY 1.21 TOOLTIP "Alla plannr väljs"
     FONT 11.

DEFINE BUTTON BTN_AVBRYT 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BACK 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade tas bort från vallistan".

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OVER 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "":L 
     SIZE 4 BY 1.21 TOOLTIP "Markerade väljs".

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_ANV FOR 
      anvandartemp SCROLLING.

DEFINE QUERY BRW_ANVOMR FOR 
      omranvandartemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_ANV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ANV C-Win _STRUCTURED
  QUERY BRW_ANV NO-LOCK DISPLAY
      anvandartemp.ANVANDARE COLUMN-LABEL "Användare" FORMAT "x(256)":U
            WIDTH 12
      anvandartemp.AV-NAMN COLUMN-LABEL "Användarnamn" FORMAT "x(256)":U
            WIDTH 34.13
      anvandartemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(5)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 56.13 BY 22.25
         FONT 4
         TITLE "Användare".

DEFINE BROWSE BRW_ANVOMR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ANVOMR C-Win _STRUCTURED
  QUERY BRW_ANVOMR NO-LOCK DISPLAY
      omranvandartemp.ANVANDARE COLUMN-LABEL "Användare" FORMAT "x(256)":U
            WIDTH 12
      omranvandartemp.AV-NAMN COLUMN-LABEL "Användarnamn" FORMAT "x(256)":U
            WIDTH 34.13
      omranvandartemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(5)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 56.13 BY 22.25
         FONT 4
         TITLE "Ansvariga".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BRW_ANV AT ROW 2.5 COL 1.88
     BRW_ANVOMR AT ROW 2.5 COL 63.5
     BTN_ALLOVER AT ROW 7.5 COL 59
     BTN_OVER AT ROW 9.67 COL 59
     BTN_BACK AT ROW 11.88 COL 59
     BTN_ALLBACK AT ROW 14.08 COL 59
     BTN_OK AT ROW 28 COL 90.75
     BTN_AVBRYT AT ROW 28 COL 105.75
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 120.88 BY 28.5.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: anvandartemp T "?" NO-UNDO temp-db anvandartemp
      TABLE: omranvandartemp T "?" NO-UNDO temp-db omranvandartemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Behörighet"
         HEIGHT             = 28.5
         WIDTH              = 120.88
         MAX-HEIGHT         = 28.5
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 28.5
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BRW_ANV 1 DEFAULT-FRAME */
/* BROWSE-TAB BRW_ANVOMR BRW_ANV DEFAULT-FRAME */
ASSIGN 
       BRW_ANV:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       BRW_ANVOMR:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ANV
/* Query rebuild information for BROWSE BRW_ANV
     _TblList          = "Temp-Tables.anvandartemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.anvandartemp.ANVANDARE
"anvandartemp.ANVANDARE" "Användare" "x(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.anvandartemp.AV-NAMN
"anvandartemp.AV-NAMN" "Användarnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "34.13" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.anvandartemp.PERSONALKOD
"anvandartemp.PERSONALKOD" "Enhet/!Sign" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_ANV */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ANVOMR
/* Query rebuild information for BROWSE BRW_ANVOMR
     _TblList          = "Temp-Tables.omranvandartemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.omranvandartemp.ANVANDARE
"omranvandartemp.ANVANDARE" "Användare" "x(256)" "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.omranvandartemp.AV-NAMN
"omranvandartemp.AV-NAMN" "Användarnamn" "x(256)" "character" ? ? ? ? ? ? no ? no no "34.13" yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.omranvandartemp.PERSONALKOD
"omranvandartemp.PERSONALKOD" "Enhet/!Sign" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_ANVOMR */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Behörighet */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Behörighet */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVBRYT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVBRYT C-Win
ON CHOOSE OF BTN_AVBRYT IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
   MESSAGE "OBS! Vill du spara dina ändringar?"
    VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO-CANCEL TITLE "Spara ändringar?" UPDATE svar AS LOGICAL.         
   IF svar THEN DO:      
      APPLY "CHOOSE" TO BTN_OK IN FRAME {&FRAME-NAME}.
   END.
   ELSE DO:
      APPLY "CLOSE":U TO THIS-PROCEDURE.
      RETURN.
   END.
   
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK C-Win
ON CHOOSE OF BTN_OK IN FRAME DEFAULT-FRAME /* Ok */
DO:  
   RUN omrsparhamtaao_UI IN anvapph (INPUT aonrvar,INPUT delnrvar,INPUT TABLE omranvandartemp). 
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_ANV
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
   {BORTBRWPROC.I}
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
   {ALLSTARTDYN.I}
   RUN enable_UI.
   {FRMSIZE.I}  
   RUN hamta_UI IN anvapph (INPUT TRUE,OUTPUT TABLE anvandartemp). 
   RUN openbdyn_UI IN brwproc[2] (INPUT "").
   RUN openbdyn_UI IN brwproc[3] (INPUT "").
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
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RUN DYNBRW.P PERSISTENT SET brwproc[2] (INPUT BRW_ANV:HANDLE IN FRAME {&FRAME-NAME} ).
   RUN DYNBRW.P PERSISTENT SET brwproc[3] (INPUT BRW_ANVOMR:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNARROW.P PERSISTENT SET brwproc[4] (INPUT BRW_ANV:HANDLE,INPUT BRW_ANVOMR:HANDLE,
                                             INPUT BTN_OVER:HANDLE, INPUT BTN_ALLOVER:HANDLE,
                                             INPUT BTN_ALLBACK:HANDLE, INPUT BTN_BACK:HANDLE).

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
  ENABLE BRW_ANV BRW_ANVOMR BTN_ALLOVER BTN_OVER BTN_BACK BTN_ALLBACK BTN_OK 
         BTN_AVBRYT 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

