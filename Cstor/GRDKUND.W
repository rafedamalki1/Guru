&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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

/* Local Variable Definitions ---                                       */
{ALLDEF.I}

DEFINE SHARED VARIABLE distnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE distproch AS HANDLE NO-UNDO. /*Handle till DISTAPP.P*/
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE avbrytvar AS LOGICAL NO-UNDO.

DEFINE TEMP-TABLE grdata_temp
   FIELD SPANID      AS INTEGER  
   FIELD NAMN        AS CHARACTER
   FIELD ANTAL       AS INTEGER   
   FIELD ANTALBLAND  AS INTEGER  
   FIELD ANTALLUFT   AS INTEGER 
   FIELD ANTALKABEL  AS INTEGER.

&SCOPED-DEFINE NEW
&SCOPED-DEFINE SHARED SHARED 
{STORTEMP.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_SPAN2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES grdata_temp

/* Definitions for BROWSE BRW_SPAN2                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_SPAN2 grdata_temp.NAMN ~
grdata_temp.ANTALBLAND grdata_temp.ANTALLUFT grdata_temp.ANTALKABEL ~
grdata_temp.ANTAL 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_SPAN2 grdata_temp.ANTALBLAND ~
grdata_temp.ANTALLUFT grdata_temp.ANTALKABEL grdata_temp.ANTAL 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_SPAN2 grdata_temp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_SPAN2 grdata_temp
&Scoped-define QUERY-STRING-BRW_SPAN2 FOR EACH grdata_temp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_SPAN2 OPEN QUERY BRW_SPAN2 FOR EACH grdata_temp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_SPAN2 grdata_temp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_SPAN2 grdata_temp


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_SPAN2 BTN_OK BTN_AVB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_SPAN2 FOR 
      grdata_temp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_SPAN2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_SPAN2 C-Win _STRUCTURED
  QUERY BRW_SPAN2 DISPLAY
      grdata_temp.NAMN COLUMN-LABEL "Spänningsnivå" FORMAT "X(35)":U
      grdata_temp.ANTALBLAND COLUMN-LABEL "Blandnät" FORMAT ">>>>>>9":U
      grdata_temp.ANTALLUFT COLUMN-LABEL "Luftl.nät" FORMAT ">>>>>>9":U
      grdata_temp.ANTALKABEL COLUMN-LABEL "Kabelnät" FORMAT ">>>>>>9":U
      grdata_temp.ANTAL COLUMN-LABEL "Totalt" FORMAT ">>>>>>9":U
  ENABLE
      grdata_temp.ANTALBLAND
      grdata_temp.ANTALLUFT
      grdata_temp.ANTALKABEL
      grdata_temp.ANTAL
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 77.5 BY 13
         TITLE "Antal kunder / spänningsnivå".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BRW_SPAN2 AT ROW 1.5 COL 1.5
     BTN_OK AT ROW 15 COL 50
     BTN_AVB AT ROW 15 COL 65
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 79.13 BY 15.38.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO TEMP-DB grdata_temp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 15.38
         WIDTH              = 79.13
         MAX-HEIGHT         = 17.88
         MAX-WIDTH          = 94.63
         VIRTUAL-HEIGHT     = 17.88
         VIRTUAL-WIDTH      = 94.63
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
/* BROWSE-TAB BRW_SPAN2 1 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_SPAN2
/* Query rebuild information for BROWSE BRW_SPAN2
     _TblList          = "Temp-Tables.grdata_temp"
     _FldNameList[1]   > Temp-Tables.grdata_temp.NAMN
"grdata_temp.NAMN" "Spänningsnivå" "X(35)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.grdata_temp.ANTALBLAND
"grdata_temp.ANTALBLAND" "Blandnät" ">>>>>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.grdata_temp.ANTALLUFT
"grdata_temp.ANTALLUFT" "Luftl.nät" ">>>>>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.grdata_temp.ANTALKABEL
"grdata_temp.ANTALKABEL" "Kabelnät" ">>>>>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.grdata_temp.ANTAL
"grdata_temp.ANTAL" "Totalt" ">>>>>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_SPAN2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   APPLY "CLOSE":U TO THIS-PROCEDURE.
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


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
   MESSAGE "OBS! Vill du spara dina ändringar?"
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO-CANCEL TITLE "Spara ändringar?" UPDATE svar AS LOGICAL.         
   IF svar THEN DO:
      avbrytvar = TRUE.
      APPLY "CHOOSE" TO BTN_OK IN FRAME {&FRAME-NAME}.
   END.
   ELSE IF NOT svar THEN DO:
      musz = TRUE.             
      APPLY "CLOSE":U TO THIS-PROCEDURE.   
      RETURN NO-APPLY.
   END.                    
   ELSE DO:
      musz = musz.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK C-Win
ON CHOOSE OF BTN_OK IN FRAME DEFAULT-FRAME /* Ok */
DO:
   RUN grdkundspara_UI IN distproch 
      (INPUT distnr, INPUT stordistemp.ARTAL, INPUT-OUTPUT TABLE grdata_temp).
   musz = FALSE.
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_SPAN2
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
   FIND stordistemp WHERE stordistemp.DISTRIKTID = distnr NO-LOCK NO-ERROR.
   {&WINDOW-NAME}:TITLE = "Grunddata kunder för distrikt " + stordistemp.NAMN + " årtal " + STRING(stordistemp.ARTAL).   
   RUN grdkundhamta_UI IN distproch 
      (INPUT distnr, INPUT stordistemp.ARTAL, INPUT-OUTPUT TABLE grdata_temp).
   RUN enable_UI.   
   {FRMSIZE.I}      
   OPEN QUERY BRW_SPAN2 FOR EACH grdata_temp NO-LOCK.  
   {musarrow.i}
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
  ENABLE BRW_SPAN2 BTN_OK BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

