&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
&Scoped-define NEW
&Scoped-define SHARED 
{STORTEMP.I}
DEFINE VARIABLE kodvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE kodtyp AS CHARACTER NO-UNDO.
DEFINE VARIABLE posch AS CHARACTER NO-UNDO.
DEFINE VARIABLE inrec AS RECID NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE BUFFER inlastbuff FOR inlastabtemp.
DEFINE VARIABLE stornapph AS HANDLE NO-UNDO.
DEFINE TEMP-TABLE einlastabtemp NO-UNDO LIKE inlastabtemp.     
DEFINE TEMP-TABLE fnamn NO-UNDO     
    FIELD fnamnet AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_INL

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES inlastabtemp

/* Definitions for BROWSE BRW_INL                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_INL inlastabtemp.INKOD ~
inlastabtemp.INKODTYP inlastabtemp.INKODPOSCH inlastabtemp.NAMN ~
inlastabtemp.TABNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_INL inlastabtemp.INKOD 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_INL inlastabtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_INL inlastabtemp
&Scoped-define QUERY-STRING-BRW_INL FOR EACH inlastabtemp NO-LOCK ~
    BY inlastabtemp.INKOD ~
       BY inlastabtemp.INKODTYP ~
        BY inlastabtemp.INKODPOSCH
&Scoped-define OPEN-QUERY-BRW_INL OPEN QUERY BRW_INL FOR EACH inlastabtemp NO-LOCK ~
    BY inlastabtemp.INKOD ~
       BY inlastabtemp.INKODTYP ~
        BY inlastabtemp.INKODPOSCH.
&Scoped-define TABLES-IN-QUERY-BRW_INL inlastabtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_INL inlastabtemp


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BRW_INL}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_INL SEL_TABB BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS SEL_TABB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-GO 
     LABEL "Ok" 
     SIZE 15 BY 1.13.

DEFINE VARIABLE SEL_TABB AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 14.63 BY 12.54 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_INL FOR 
      inlastabtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_INL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_INL C-Win _STRUCTURED
  QUERY BRW_INL NO-LOCK DISPLAY
      inlastabtemp.INKOD COLUMN-LABEL "In!kod" FORMAT "X(3)":U
      inlastabtemp.INKODTYP COLUMN-LABEL "In!kod!typ" FORMAT "X(2)":U
      inlastabtemp.INKODPOSCH COLUMN-LABEL "In!kod!pos" FORMAT "X(4)":U
      inlastabtemp.NAMN COLUMN-LABEL "Namn" FORMAT "X(45)":U
      inlastabtemp.TABNAMN COLUMN-LABEL "Tabnamn" FORMAT "X(19)":U
  ENABLE
      inlastabtemp.INKOD
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 82.13 BY 12.92.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BRW_INL AT ROW 1.63 COL 1.13
     SEL_TABB AT ROW 1.75 COL 84.13 NO-LABEL
     BTN_AVB AT ROW 15.58 COL 29.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 97.75 BY 16.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: inlastabtemp T "?" NO-UNDO temp-db inlastabtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 16
         WIDTH              = 98
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 98
         VIRTUAL-HEIGHT     = 16
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
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_INL 1 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_INL
/* Query rebuild information for BROWSE BRW_INL
     _TblList          = "Temp-Tables.inlastabtemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.inlastabtemp.INKOD|yes,Temp-Tables.inlastabtemp.INKODTYP|yes,Temp-Tables.inlastabtemp.INKODPOSCH|yes"
     _FldNameList[1]   > Temp-Tables.inlastabtemp.INKOD
"inlastabtemp.INKOD" "In!kod" "X(3)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.inlastabtemp.INKODTYP
"inlastabtemp.INKODTYP" "In!kod!typ" "X(2)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.inlastabtemp.INKODPOSCH
"inlastabtemp.INKODPOSCH" "In!kod!pos" "X(4)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.inlastabtemp.NAMN
"inlastabtemp.NAMN" "Namn" "X(45)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.inlastabtemp.TABNAMN
"inlastabtemp.TABNAMN" "Tabnamn" "X(19)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_INL */
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


&Scoped-define BROWSE-NAME BRW_INL
&Scoped-define SELF-NAME BRW_INL
&Scoped-define SELF-NAME inlastabtemp.INKOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inlastabtemp.INKOD BRW_INL _BROWSE-COLUMN C-Win
ON ENTRY OF inlastabtemp.INKOD IN BROWSE BRW_INL /* In!kod */
DO:
  DISPLAY inlastabtemp.INKOD WITH BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inlastabtemp.INKOD BRW_INL _BROWSE-COLUMN C-Win
ON LEAVE OF inlastabtemp.INKOD IN BROWSE BRW_INL /* In!kod */
DO:
  DO TRANSACTION:
      status-ok = {&BROWSE-NAME}:REFRESH() IN FRAME {&FRAME-NAME}.
      GET CURRENT {&BROWSE-NAME} EXCLUSIVE-LOCK.
      inlastabtemp.INKOD = INPUT BROWSE {&BROWSE-NAME} inlastabtemp.INKOD.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inlastabtemp.INKODTYP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inlastabtemp.INKODTYP BRW_INL _BROWSE-COLUMN C-Win
ON LEAVE OF inlastabtemp.INKODTYP IN BROWSE BRW_INL /* In!kod!typ */
DO:
  DO TRANSACTION:
      status-ok = {&BROWSE-NAME}:REFRESH() IN FRAME {&FRAME-NAME}.
      GET CURRENT {&BROWSE-NAME} EXCLUSIVE-LOCK.
      inlastabtemp.INKODTYP = INPUT BROWSE {&BROWSE-NAME} inlastabtemp.INKODTYP.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inlastabtemp.INKODPOSCH
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inlastabtemp.INKODPOSCH BRW_INL _BROWSE-COLUMN C-Win
ON LEAVE OF inlastabtemp.INKODPOSCH IN BROWSE BRW_INL /* In!kod!pos */
DO:
  DO TRANSACTION:
      status-ok = {&BROWSE-NAME}:REFRESH() IN FRAME {&FRAME-NAME}.
      GET CURRENT {&BROWSE-NAME} EXCLUSIVE-LOCK.
      inlastabtemp.INKODPOSCH = INPUT BROWSE {&BROWSE-NAME} inlastabtemp.INKODPOSCH.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inlastabtemp.NAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inlastabtemp.NAMN BRW_INL _BROWSE-COLUMN C-Win
ON LEAVE OF inlastabtemp.NAMN IN BROWSE BRW_INL /* Namn */
DO:
  DO TRANSACTION:
      status-ok = {&BROWSE-NAME}:REFRESH() IN FRAME {&FRAME-NAME}.
      GET CURRENT {&BROWSE-NAME} EXCLUSIVE-LOCK.
      inlastabtemp.NAMN = INPUT BROWSE {&BROWSE-NAME} inlastabtemp.NAMN.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME inlastabtemp.TABNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inlastabtemp.TABNAMN BRW_INL _BROWSE-COLUMN C-Win
ON ANY-KEY OF inlastabtemp.TABNAMN IN BROWSE BRW_INL /* Tabnamn */
DO:
  IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:         
      APPLY "LEAVE" TO inlastabtemp.TABNAMN IN BROWSE {&BROWSE-NAME}.               
      IF NOT AVAILABLE inlastabtemp THEN DO:
         FIND inlastabtemp WHERE RECID(inlastabtemp) = inrec NO-LOCK NO-ERROR.
      END.
      APPLY "ENTRY" TO inlastabtemp.INKOD IN BROWSE {&BROWSE-NAME}.
      RETURN NO-APPLY.         
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inlastabtemp.TABNAMN BRW_INL _BROWSE-COLUMN C-Win
ON ENDKEY OF inlastabtemp.TABNAMN IN BROWSE BRW_INL /* Tabnamn */
DO:
  APPLY "ENTRY" TO inlastabtemp.INKOD IN BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inlastabtemp.TABNAMN BRW_INL _BROWSE-COLUMN C-Win
ON ENTRY OF inlastabtemp.TABNAMN IN BROWSE BRW_INL /* Tabnamn */
DO:
  IF inlastabtemp.TABNAMN = "" THEN DO:
      IF inlastabtemp.INKODTYP NE "" THEN DO:
         IF inlastabtemp.INKODPOSCH NE "" THEN DO TRANSACTION:
            status-ok = {&BROWSE-NAME}:REFRESH() IN FRAME {&FRAME-NAME}.
            GET CURRENT {&BROWSE-NAME} EXCLUSIVE-LOCK.
            inlastabtemp.TABNAMN = SEL_TABB. 
            DISPLAY inlastabtemp.TABNAMN WITH BROWSE {&BROWSE-NAME}.          
         END.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL inlastabtemp.TABNAMN BRW_INL _BROWSE-COLUMN C-Win
ON LEAVE OF inlastabtemp.TABNAMN IN BROWSE BRW_INL /* Tabnamn */
DO:
   DO TRANSACTION:
      status-ok = {&BROWSE-NAME}:REFRESH() IN FRAME {&FRAME-NAME}.
      GET CURRENT {&BROWSE-NAME} EXCLUSIVE-LOCK.
      inlastabtemp.TABNAMN = INPUT BROWSE {&BROWSE-NAME} inlastabtemp.TABNAMN.
   END.
   RUN ngnkey_UI.
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.     
   IF musz = TRUE THEN DO:
      musz = FALSE.
      APPLY "ENDKEY" TO inlastabtemp.TABNAMN IN BROWSE {&BROWSE-NAME}.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Ok */
DO:
/*    FOR EACH INLASTAB WHERE INLASTAB.INKOD = "" : */
/*       DELETE INLASTAB.                           */
/*    END.                                          */
   APPLY "GO" TO BTN_AVB.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON GO OF BTN_AVB IN FRAME DEFAULT-FRAME /* Ok */
DO:
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_TABB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_TABB C-Win
ON VALUE-CHANGED OF SEL_TABB IN FRAME DEFAULT-FRAME
DO:
   SEL_TABB = INPUT SEL_TABB.
   DO TRANSACTION:
      GET CURRENT {&BROWSE-NAME} EXCLUSIVE-LOCK.
      inlastabtemp.TABNAM = SEL_TABB.
   END. 
   DISPLAY inlastabtemp.TABNAM WITH BROWSE {&BROWSE-NAME}.
   APPLY "ENTRY" TO inlastabtemp.TABNAM IN BROWSE {&BROWSE-NAME}.
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
ON CLOSE OF THIS-PROCEDURE DO:
   IF VALID-HANDLE(stornapph) THEN DELETE PROCEDURE stornapph.
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
   RUN inlasthmt_UI IN stornapph (OUTPUT TABLE fnamn,OUTPUT TABLE inlastabtemp).             
   /*FIND FIRST INLASTAB NO-LOCK NO-ERROR.
   IF NOT AVAILABLE INLASTAB THEN DO TRANSACTION:
      CREATE INLASTAB.
      ASSIGN
      INLASTAB.INLASTNR = 1
      INLASTAB.INKODID = "1".
   END.
   FOR EACH RT9._FILE  NO-LOCK:
      IF SUBSTRING(RT9._FILE._FILE-NAME,1,1) NE "_" THEN DO:
         status-ok = SEL_TABB:ADD-LAST(RT9._FILE._FILE-NAME).
      END.
   END.*/
   FOR EACH fnamn  NO-LOCK:    
      status-ok = SEL_TABB:ADD-LAST(fnamn.fnamnet).    
   END.

   RUN enable_UI.   
   {FRMSIZE.I}    
   {musarrow.i}
   {WIN_M_SLUT.I}
   GET FIRST {&BROWSE-NAME} NO-LOCK.   
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
   
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_INL:HANDLE IN FRAME {&FRAME-NAME}).
   IF Guru.Konstanter:appcon THEN DO:
      RUN STORINAPP.P PERSISTENT SET stornapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN STORINAPP.P PERSISTENT SET stornapph.
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
  DISPLAY SEL_TABB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BRW_INL SEL_TABB BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ngnkey_UI C-Win 
PROCEDURE ngnkey_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {TRYCKS.I}   
   IF /*KEYFUNCTION(LASTKEY) = ("TAB") OR*/  KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:            
      ASSIGN
      kodvar = inlastabtemp.INKOD
      kodtyp = inlastabtemp.INKODTYP
      posch = inlastabtemp.INKODPOSCH.

      FIND LAST inlastbuff USE-INDEX INLAST NO-LOCK NO-ERROR.           
      DO TRANSACTION:   
         CREATE inlastabtemp.   
         ASSIGN    
         inlastabtemp.INKOD = kodvar 
         inlastabtemp.INKODTYP = kodtyp
         inlastabtemp.INKODPOSCH = posch
         inlastabtemp.INLASTNR = inlastbuff.INLASTNR + 1
         inlastabtemp.INKODID = STRING(inlastbuff.INLASTNR + 1). 
         inrec = RECID(inlastabtemp).                  
      END.             
      RELEASE inlastbuff.
      RUN openbdynspec_UI IN brwproc[1].
      FIND FIRST inlastabtemp NO-LOCK NO-ERROR.
      IF AVAILABLE inlastabtemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(inlastabtemp)).
         RUN lastselectdyn_UI IN brwproc[1].
      END.
      EMPTY TEMP-TABLE einlastabtemp.
      CREATE einlastabtemp.
      BUFFER-COPY inlastabtemp TO einlastabtemp.
      RUN spinlast_UI IN stornapph (INPUT TABLE einlastabtemp).             
      /*OPEN QUERY {&BROWSE-NAME} FOR EACH INLASTAB NO-LOCK 
      BY INLASTAB.INKOD BY INLASTAB.INKODTYP BY INLASTAB.INKODPOSCH BY INLASTAB.INLASTNR.            */
      /*RUN repo_UI (INPUT inrec).            */
      musz = TRUE.
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

