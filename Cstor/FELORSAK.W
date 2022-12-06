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
{GLOBVAR2DEL1.I}
DEFINE NEW SHARED VARIABLE franar AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE prec AS RECID NO-UNDO.

DEFINE NEW SHARED VARIABLE distproch AS HANDLE NO-UNDO. /*Handle till FELORSAKAPP.P*/
DEFINE NEW SHARED VARIABLE distnr AS INTEGER NO-UNDO.

DEFINE NEW SHARED VARIABLE andra AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE forvar AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE natvar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE VARIABLE uppar AS INTEGER NO-UNDO. 
DEFINE VARIABLE slutar AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.

&SCOPED-DEFINE NEW
&SCOPED-DEFINE SHARED SHARED 
{STORTEMP.I}
{STRTEMP.I}
{AVDELNINGTEMP.I}

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
&Scoped-define INTERNAL-TABLES felorsaktemp

/* Definitions for BROWSE BRW_P1                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_P1 felorsaktemp.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_P1 
&Scoped-define QUERY-STRING-BRW_P1 FOR EACH felorsaktemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_P1 OPEN QUERY BRW_P1 FOR EACH felorsaktemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_P1 felorsaktemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_P1 felorsaktemp


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_P1 BTN_NY BTN_AND BTN_BORT BTN_AVB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AND 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny" 
     SIZE 12 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_P1 FOR 
      felorsaktemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_P1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_P1 C-Win _STRUCTURED
  QUERY BRW_P1 NO-LOCK DISPLAY
      felorsaktemp.NAMN COLUMN-LABEL "Namn" FORMAT "X(256)":U WIDTH 36
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 39.5 BY 11.58
         TITLE "Egna felorsaker".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BRW_P1 AT ROW 1.92 COL 1.5
     BTN_NY AT ROW 13.75 COL 2
     BTN_AND AT ROW 13.75 COL 15.25
     BTN_BORT AT ROW 13.75 COL 28.5
     BTN_AVB AT ROW 13.75 COL 42.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 56.63 BY 14.04.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: felorsaktemp T "?" NO-UNDO temp-db felorsaktemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Administration egna felorsaker"
         HEIGHT             = 14
         WIDTH              = 56
         MAX-HEIGHT         = 23.5
         MAX-WIDTH          = 114.25
         VIRTUAL-HEIGHT     = 23.5
         VIRTUAL-WIDTH      = 114.25
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
/* BROWSE-TAB BRW_P1 1 DEFAULT-FRAME */
ASSIGN 
       BRW_P1:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE
       BRW_P1:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 1000
       BRW_P1:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE
       BRW_P1:COLUMN-RESIZABLE IN FRAME DEFAULT-FRAME       = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_P1
/* Query rebuild information for BROWSE BRW_P1
     _TblList          = "Temp-Tables.felorsaktemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.felorsaktemp.NAMN
"NAMN" "Namn" "X(256)" "character" ? ? ? ? ? ? no ? no no "36" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_P1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Administration egna felorsaker */
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
ON WINDOW-CLOSE OF C-Win /* Administration egna felorsaker */
DO:
  /* This event will close the window and terminate the procedure.  */
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_P1
&Scoped-define SELF-NAME BRW_P1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_P1 C-Win
ON VALUE-CHANGED OF BRW_P1 IN FRAME DEFAULT-FRAME /* Egna felorsaker */
DO:
   IF AVAILABLE felorsaktemp THEN distnr = felorsaktemp.FELOID.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AND C-Win
ON CHOOSE OF BTN_AND IN FRAME DEFAULT-FRAME /* Ändra */
DO:
   IF AVAILABLE felorsaktemp THEN DO:      
      
      andra = TRUE.
      RUN NYFELORSAK.W.
      
      andra = FALSE.
      IF musz = FALSE THEN DO:      
         RUN openbdynspec_UI IN brwproc[1].
         FIND FIRST felorsaktemp WHERE felorsaktemp.FELOID = distnr NO-LOCK NO-ERROR.          
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(felorsaktemp)).
         RUN lastselectdyn_UI IN brwproc[1].         
      END.           
      ELSE DO:
         musz = FALSE.
      END.
   END.
   ELSE DO:
      MESSAGE "Det finns ingen felorsak att ändra."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT C-Win
ON CHOOSE OF BTN_BORT IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
   {muswait.i}
   IF AVAILABLE felorsaktemp THEN DO:

      MESSAGE "Vill du ta bort markerad felorsak?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Ta bort?" UPDATE svar AS LOGICAL.
      IF svar THEN DO:
         RUN stordistbtnbort_UI IN distproch (INPUT distnr).
         FIND felorsaktemp WHERE felorsaktemp.FELOID = distnr EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE felorsaktemp THEN DELETE felorsaktemp.
         RUN selnextprevrow_UI IN brwproc[1].
         RUN openbdynspec_UI IN brwproc[1].
         RUN lastselectdyn_UI IN brwproc[1].
      END.
   END.
   ELSE DO:
      MESSAGE "Det finns ingen felorsak att ta bort."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.      
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY C-Win
ON CHOOSE OF BTN_NY IN FRAME DEFAULT-FRAME /* Ny */
DO:
   {muswait.i}  
   musz = FALSE.
   
   RUN NYFELORSAK.W.
   
   IF musz = FALSE THEN DO:
      FIND FIRST felorsaktemp WHERE felorsaktemp.FELOID = distnr NO-LOCK NO-ERROR.
      IF AVAILABLE felorsaktemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(felorsaktemp)).
         RUN openbdynspec_UI IN brwproc[1].
         RUN lastselectdyn_UI IN brwproc[1].
      END.        
   END.
   ELSE DO:
      musz = FALSE.
   END.      
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
   IF VALID-HANDLE(distproch) THEN DELETE PROCEDURE distproch.
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
   RUN changequery_UI.
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
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_P1:HANDLE IN FRAME {&FRAME-NAME}). 
   IF Guru.Konstanter:appcon THEN DO:
      RUN FELORSAKAPP.P PERSISTENT SET distproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN FELORSAKAPP.P PERSISTENT SET distproch. 
   END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changequery_UI C-Win 
PROCEDURE changequery_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE kommandoquery AS CHARACTER NO-UNDO.
   kommandoquery = "felorsaktemp.FELOID >= 500".
   RUN setcolsortvar_UI IN brwproc[1] (INPUT kommandoquery).
   RUN openbdynspec_UI IN brwproc[1].
   IF AVAILABLE stordistemp THEN distnr = stordistemp.DISTRIKTID.
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
  ENABLE BRW_P1 BTN_NY BTN_AND BTN_BORT BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

