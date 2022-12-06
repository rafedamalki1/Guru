&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          rt9              PROGRESS
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

DEFINE VARIABLE kodvar LIKE INLAST.INKOD NO-UNDO.
DEFINE VARIABLE kodtyp LIKE INLASTAB.INKODTYP NO-UNDO.
DEFINE VARIABLE posch LIKE INLASTAB.INKODPOSCH NO-UNDO.
DEFINE VARIABLE inrec AS RECID NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE BUFFER inlastbuff FOR INLASTAB.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_INL

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES INLASTAB

/* Definitions for BROWSE BRW_INL                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_INL INLASTAB.INKOD INLASTAB.INKODTYP ~
INLASTAB.INKODPOSCH INLASTAB.NAMN INLASTAB.TABNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_INL INLASTAB.INKOD ~
INLASTAB.INKODTYP INLASTAB.INKODPOSCH INLASTAB.NAMN INLASTAB.TABNAMN 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_INL INLASTAB
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_INL INLASTAB
&Scoped-define OPEN-QUERY-BRW_INL OPEN QUERY BRW_INL FOR EACH INLASTAB NO-LOCK ~
    BY INLASTAB.INKOD ~
       BY INLASTAB.INKODTYP ~
        BY INLASTAB.INKODPOSCH ~
         BY INLASTAB.INLASTNR.
&Scoped-define TABLES-IN-QUERY-BRW_INL INLASTAB
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_INL INLASTAB


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
      INLASTAB SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_INL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_INL C-Win _STRUCTURED
  QUERY BRW_INL DISPLAY
      INLASTAB.INKOD COLUMN-LABEL "IN!KOD" FORMAT "X(3)":U
      INLASTAB.INKODTYP COLUMN-LABEL "IN!KOD!TYP" FORMAT "X(2)":U
      INLASTAB.INKODPOSCH COLUMN-LABEL "IN!KOD!POS" FORMAT "X(4)":U
      INLASTAB.NAMN FORMAT "X(45)":U
      INLASTAB.TABNAMN FORMAT "X(19)":U
  ENABLE
      INLASTAB.INKOD
      INLASTAB.INKODTYP
      INLASTAB.INKODPOSCH
      INLASTAB.NAMN
      INLASTAB.TABNAMN
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
                                                                        */
/* BROWSE-TAB BRW_INL 1 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_INL
/* Query rebuild information for BROWSE BRW_INL
     _TblList          = "rt9.INLASTAB"
     _OrdList          = "STU.INLASTAB.INKOD|yes,STU.INLASTAB.INKODTYP|yes,STU.INLASTAB.INKODPOSCH|yes,STU.INLASTAB.INLASTNR|yes"
     _FldNameList[1]   > rt9.INLASTAB.INKOD
"INLASTAB.INKOD" "IN!KOD" "X(3)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > rt9.INLASTAB.INKODTYP
"INLASTAB.INKODTYP" "IN!KOD!TYP" "X(2)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > rt9.INLASTAB.INKODPOSCH
"INLASTAB.INKODPOSCH" "IN!KOD!POS" "X(4)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > rt9.INLASTAB.NAMN
"INLASTAB.NAMN" ? "X(45)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > rt9.INLASTAB.TABNAMN
"INLASTAB.TABNAMN" ? "X(19)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
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
&Scoped-define SELF-NAME INLASTAB.INKOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL INLASTAB.INKOD BRW_INL _BROWSE-COLUMN C-Win
ON ENTRY OF INLASTAB.INKOD IN BROWSE BRW_INL /* IN!KOD */
DO:
   DISPLAY INLASTAB.INKOD WITH BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL INLASTAB.INKOD BRW_INL _BROWSE-COLUMN C-Win
ON LEAVE OF INLASTAB.INKOD IN BROWSE BRW_INL /* IN!KOD */
DO:
   DO TRANSACTION:
      status-ok = {&BROWSE-NAME}:REFRESH() IN FRAME {&FRAME-NAME}.
      GET CURRENT {&BROWSE-NAME} EXCLUSIVE-LOCK.
      INLASTAB.INKOD = INPUT BROWSE {&BROWSE-NAME} INLASTAB.INKOD.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME INLASTAB.INKODTYP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL INLASTAB.INKODTYP BRW_INL _BROWSE-COLUMN C-Win
ON LEAVE OF INLASTAB.INKODTYP IN BROWSE BRW_INL /* IN!KOD!TYP */
DO:
   DO TRANSACTION:
      status-ok = {&BROWSE-NAME}:REFRESH() IN FRAME {&FRAME-NAME}.
      GET CURRENT {&BROWSE-NAME} EXCLUSIVE-LOCK.
      INLASTAB.INKODTYP = INPUT BROWSE {&BROWSE-NAME} INLASTAB.INKODTYP.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME INLASTAB.INKODPOSCH
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL INLASTAB.INKODPOSCH BRW_INL _BROWSE-COLUMN C-Win
ON LEAVE OF INLASTAB.INKODPOSCH IN BROWSE BRW_INL /* IN!KOD!POS */
DO:
   DO TRANSACTION:
      status-ok = {&BROWSE-NAME}:REFRESH() IN FRAME {&FRAME-NAME}.
      GET CURRENT {&BROWSE-NAME} EXCLUSIVE-LOCK.
      INLASTAB.INKODPOSCH = INPUT BROWSE {&BROWSE-NAME} INLASTAB.INKODPOSCH.
   END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME INLASTAB.NAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL INLASTAB.NAMN BRW_INL _BROWSE-COLUMN C-Win
ON LEAVE OF INLASTAB.NAMN IN BROWSE BRW_INL /* NAMN */
DO:
   DO TRANSACTION:
      status-ok = {&BROWSE-NAME}:REFRESH() IN FRAME {&FRAME-NAME}.
      GET CURRENT {&BROWSE-NAME} EXCLUSIVE-LOCK.
      INLASTAB.NAMN = INPUT BROWSE {&BROWSE-NAME} INLASTAB.NAMN.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME INLASTAB.TABNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL INLASTAB.TABNAMN BRW_INL _BROWSE-COLUMN C-Win
ON ANY-KEY OF INLASTAB.TABNAMN IN BROWSE BRW_INL /* TABNAMN */
DO:   
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:         
      APPLY "LEAVE" TO INLASTAB.TABNAMN IN BROWSE {&BROWSE-NAME}.               
      IF NOT AVAILABLE INLASTAB THEN DO:
         FIND INLASTAB WHERE RECID(INLASTAB) = inrec NO-LOCK NO-ERROR.
      END.
      APPLY "ENTRY" TO INLASTAB.INKOD IN BROWSE {&BROWSE-NAME}.
      RETURN NO-APPLY.         
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL INLASTAB.TABNAMN BRW_INL _BROWSE-COLUMN C-Win
ON ENDKEY OF INLASTAB.TABNAMN IN BROWSE BRW_INL /* TABNAMN */
DO:
   APPLY "ENTRY" TO INLASTAB.INKOD IN BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL INLASTAB.TABNAMN BRW_INL _BROWSE-COLUMN C-Win
ON ENTRY OF INLASTAB.TABNAMN IN BROWSE BRW_INL /* TABNAMN */
DO:
   IF INLASTAB.TABNAMN = "" THEN DO:
      IF INLASTAB.INKODTYP NE "" THEN DO:
         IF INLASTAB.INKODPOSCH NE "" THEN DO TRANSACTION:
            status-ok = {&BROWSE-NAME}:REFRESH() IN FRAME {&FRAME-NAME}.
            GET CURRENT {&BROWSE-NAME} EXCLUSIVE-LOCK.
            INLASTAB.TABNAMN = SEL_TABB. 
            DISPLAY INLASTAB.TABNAMN WITH BROWSE {&BROWSE-NAME}.          
         END.
      END.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL INLASTAB.TABNAMN BRW_INL _BROWSE-COLUMN C-Win
ON LEAVE OF INLASTAB.TABNAMN IN BROWSE BRW_INL /* TABNAMN */
DO:
   DO TRANSACTION:
      status-ok = {&BROWSE-NAME}:REFRESH() IN FRAME {&FRAME-NAME}.
      GET CURRENT {&BROWSE-NAME} EXCLUSIVE-LOCK.
      INLASTAB.TABNAMN = INPUT BROWSE {&BROWSE-NAME} INLASTAB.TABNAMN.
   END.
   RUN ngnkey_UI.
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.     
   IF musz = TRUE THEN DO:
      musz = FALSE.
      APPLY "ENDKEY" TO INLASTAB.TABNAMN IN BROWSE {&BROWSE-NAME}.
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
      INLASTAB.TABNAM = SEL_TABB.
   END. 
   DISPLAY INLASTAB.TABNAM WITH BROWSE {&BROWSE-NAME}.
   APPLY "ENTRY" TO INLASTAB.TABNAM IN BROWSE {&BROWSE-NAME}.
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
   FIND FIRST INLASTAB NO-LOCK NO-ERROR.
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
      kodvar = INLASTAB.INKOD
      kodtyp = INLASTAB.INKODTYP
      posch = INLASTAB.INKODPOSCH.

      FIND LAST inlastbuff USE-INDEX INLAST NO-LOCK NO-ERROR.           
      DO TRANSACTION:   
         CREATE INLASTAB.   
         ASSIGN    
         INLASTAB.INKOD = kodvar 
         INLASTAB.INKODTYP = kodtyp
         INLASTAB.INKODPOSCH = posch
         INLASTAB.INLASTNR = inlastbuff.INLASTNR + 1
         INLASTAB.INKODID = STRING(inlastbuff.INLASTNR + 1). 
         inrec = RECID(INLASTAB).                  
      END.             
      RELEASE inlastbuff.
      OPEN QUERY {&BROWSE-NAME} FOR EACH INLASTAB NO-LOCK 
      BY INLASTAB.INKOD BY INLASTAB.INKODTYP BY INLASTAB.INKODPOSCH BY INLASTAB.INLASTNR.            
      RUN repo_UI (INPUT inrec).            
      musz = TRUE.
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo_UI C-Win 
PROCEDURE repo_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER browrec AS RECID NO-UNDO.
   {&BROWSE-NAME}:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.                        
   REPOSITION {&BROWSE-NAME} TO RECID browrec.
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

