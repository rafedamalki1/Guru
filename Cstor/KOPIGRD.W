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

DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE forvar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE distnr AS INTEGER NO-UNDO.
DEFINE VARIABLE distartal AS INTEGER NO-UNDO.
DEFINE VARIABLE distnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE uppar AS INTEGER NO-UNDO. 
DEFINE VARIABLE slutar AS INTEGER NO-UNDO.
DEFINE VARIABLE franar AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.

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
&Scoped-define BROWSE-NAME BRW_P1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES stordistemp

/* Definitions for BROWSE BRW_P1                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_P1 stordistemp.VIDISTRIKT ~
stordistemp.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_P1 stordistemp.VIDISTRIKT 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_P1 stordistemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_P1 stordistemp
&Scoped-define QUERY-STRING-BRW_P1 FOR EACH stordistemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_P1 OPEN QUERY BRW_P1 FOR EACH stordistemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_P1 stordistemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_P1 stordistemp


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CMB_ARTAL BRW_P1 BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS CMB_ARTAL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE CMB_ARTAL AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Årtal" 
     VIEW-AS COMBO-BOX INNER-LINES 7
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 12.25 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_P1 FOR 
      stordistemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_P1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_P1 C-Win _STRUCTURED
  QUERY BRW_P1 NO-LOCK DISPLAY
      stordistemp.VIDISTRIKT COLUMN-LABEL "Nr" FORMAT "X(3)":U
      stordistemp.NAMN COLUMN-LABEL "Namn" FORMAT "X(30)":U
  ENABLE
      stordistemp.VIDISTRIKT
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 36.5 BY 11.58
         TITLE "Distrikt".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     CMB_ARTAL AT ROW 3 COL 1.75
     BRW_P1 AT ROW 4.38 COL 1.5
     BTN_OK AT ROW 16.25 COL 24
     BTN_AVB AT ROW 16.25 COL 39
     "Markera det distrikt som Ni vill kopiera grunddata från." VIEW-AS TEXT
          SIZE 56 BY 1.38 AT ROW 1.5 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 97.63 BY 18.95.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: stordistemp T "?" NO-UNDO temp-db stordistemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Administration distrikt"
         HEIGHT             = 16.54
         WIDTH              = 57.5
         MAX-HEIGHT         = 23.5
         MAX-WIDTH          = 98
         VIRTUAL-HEIGHT     = 23.5
         VIRTUAL-WIDTH      = 98
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
/* BROWSE-TAB BRW_P1 CMB_ARTAL DEFAULT-FRAME */
ASSIGN 
       BRW_P1:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 1000.

/* SETTINGS FOR COMBO-BOX CMB_ARTAL IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_P1
/* Query rebuild information for BROWSE BRW_P1
     _TblList          = "Temp-Tables.stordistemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.stordistemp.VIDISTRIKT
"stordistemp.VIDISTRIKT" "Nr" "X(3)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.stordistemp.NAMN
"stordistemp.NAMN" "Namn" "X(30)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_P1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Administration distrikt */
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
ON WINDOW-CLOSE OF C-Win /* Administration distrikt */
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
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK C-Win
ON CHOOSE OF BTN_OK IN FRAME DEFAULT-FRAME /* Ok */
DO:
   FIND FIRST stordistemp WHERE stordistemp.AVDELNINGNR = forvar
   AND stordistemp.ARTAL = franar NO-LOCK NO-ERROR.
   IF AVAILABLE stordistemp THEN DO:
      ASSIGN
      status-ok = BRW_P1:SELECT-FOCUSED-ROW().
      IF stordistemp.DISTRIKTID NE distnr THEN DO:      
         MESSAGE "Är det korrekt att Ni vill kopiera grundata från: " + stordistemp.NAMN + " årtal: " +
         STRING(stordistemp.ARTAL) + " till: " + distnamn + " årtal: " + 
         STRING(distartal) + "?" VIEW-AS ALERT-BOX
         QUESTION BUTTONS YES-NO TITLE "Kopiera?" UPDATE svar AS LOGICAL.         
         IF svar THEN DO:
            IF Guru.Konstanter:appcon THEN DO:                           
               RUN STORDEL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
               (INPUT distnr, INPUT distartal).      
            END.
            ELSE DO:
               RUN STORDEL.P 
               (INPUT distnr, INPUT distartal).
            END.
            IF Guru.Konstanter:appcon THEN DO:                           
               RUN STORKOP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
               (INPUT distnr, INPUT distartal, INPUT stordistemp.DISTRIKTID, INPUT stordistemp.ARTAL).      
            END.
            ELSE DO:
               RUN STORKOP.P 
               (INPUT distnr, INPUT distartal, INPUT stordistemp.DISTRIKTID, INPUT stordistemp.ARTAL).
            END.
            APPLY "CLOSE":U TO THIS-PROCEDURE.
         END.
         ELSE DO:
            RETURN NO-APPLY.
         END.
      END.
      ELSE DO:
         MESSAGE "Ni kan inte kopiera grunddata från samma distrikt som Ni skall kopiera till."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
      END.
   END.
   ELSE DO:
      MESSAGE "Ni har inte marekerat något distrikt att kopiera grunddata från."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_ARTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_ARTAL C-Win
ON VALUE-CHANGED OF CMB_ARTAL IN FRAME DEFAULT-FRAME /* Årtal */
DO:                           
   ASSIGN
   CMB_ARTAL = INPUT CMB_ARTAL
   franar = CMB_ARTAL.     
   RUN changequery_UI.
/*    OPEN QUERY BRW_P1 FOR EACH stordistemp WHERE stordistemp.AVDELNINGNR = forvar */
/*    AND stordistemp.ARTAL = franar USE-INDEX AVDARTAL NO-LOCK.                    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_P1
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
   {ALLSTARTDYN.I} 
   {muswait.i}
   FIND FIRST stordistemp WHERE stordistemp.DISTRIKTID = distnr NO-LOCK NO-ERROR.
   IF AVAILABLE stordistemp THEN DO: 
      ASSIGN
      distnamn = stordistemp.NAMN
      distartal = stordistemp.ARTAL.
   END.
   FIND FIRST stordistemp WHERE stordistemp.AVDELNINGNR = forvar
   USE-INDEX ARTAL NO-LOCK NO-ERROR.
   IF AVAILABLE stordistemp THEN DO:      
      ASSIGN  
      uppar = stordistemp.ARTAL.
      FIND LAST stordistemp WHERE stordistemp.AVDELNINGNR = forvar
      USE-INDEX ARTAL NO-LOCK NO-ERROR.
      ASSIGN
      slutar = stordistemp.ARTAL + 1.
   END.   
   ELSE DO:
      ASSIGN
      uppar = YEAR(TODAY)
      slutar = YEAR(TODAY) + 1.
   END.
   /*LADDAR ÅR I CMB_ARTAL*/ 
   ASSIGN
   status-ok = CMB_ARTAL:DELETE("0")  
   status-ok = CMB_ARTAL:ADD-LAST(STRING(uppar,"9999")).             
   DO WHILE uppar < slutar: 
       uppar = uppar + 1.             
       status-ok = CMB_ARTAL:ADD-LAST(STRING(uppar,"9999")).    
   END.
   ASSIGN
   CMB_ARTAL:SCREEN-VALUE = STRING(slutar - 1,"9999")
   franar = slutar - 1
   CMB_ARTAL = INPUT CMB_ARTAL.
   RUN enable_UI.   
   {FRMSIZE.I}       
   RUN changequery_UI.
/*    OPEN QUERY BRW_P1 FOR EACH stordistemp WHERE stordistemp.AVDELNINGNR = forvar */
/*    AND stordistemp.ARTAL = franar USE-INDEX AVDARTAL NO-LOCK.                    */
/*    APPLY "VALUE-CHANGED" TO BRW_P1 IN FRAME {&FRAME-NAME}.                       */
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
   stordistemp.VIDISTRIKT:READ-ONLY IN BROWSE BRW_P1 = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_P1:HANDLE IN FRAME {&FRAME-NAME}). 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changequery_UI C-Win 
PROCEDURE changequery_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/                                
   DEFINE VARIABLE kommandoquery AS CHARACTER NO-UNDO.
   kommandoquery = "stordistemp.AVDELNINGNR = " + STRING(forvar) + 
      " AND stordistemp.ARTAL = " + STRING(franar).
   RUN setcolsortvar_UI IN brwproc[1] (INPUT kommandoquery).
   RUN openbdynspec_UI IN brwproc[1].   
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
  DISPLAY CMB_ARTAL 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE CMB_ARTAL BRW_P1 BTN_OK BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

