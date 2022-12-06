&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
&ANALYZE-RESUME
/* Connected Databases 
          RT8               PROGRESS
*/
&Scoped-define WINDOW-NAME    WINDOW-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/05/02 -  1:43 pm

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

DEFINE NEW SHARED VARIABLE brec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE borec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE ovrec AS RECID NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse (alphabetically)                   */
&Scoped-define FRAME-NAME  FRAME-A
&Scoped-define BROWSE-NAME BROWSE-16

/* Custom List Definitions                                              */
&Scoped-define LIST-1 
&Scoped-define LIST-2 
&Scoped-define LIST-3 

/* Definitions for BROWSE BROWSE-16                                     */
&Scoped-define FIELDS-IN-QUERY-BROWSE-16 BEREDSKAPTAB.BEREDSKAPSAVTAL ~
BEREDSKAPTAB.BEREDSKAP BEREDSKAPTAB.DAGNR BEREDSKAPTAB.BERSTART1 ~
BEREDSKAPTAB.BERSTOPP1 BEREDSKAPTAB.BERANTAL 
&Scoped-define OPEN-QUERY-BROWSE-16 OPEN QUERY BROWSE-16 FOR EACH BEREDSKAPTAB NO-LOCK INDEXED-REPOSITION.
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-16 BEREDSKAPTAB
&Scoped-define TABLES-IN-QUERY-BROWSE-16 BEREDSKAPTAB 

/* Definitions for FRAME FRAME-A                                        */
&Scoped-define FIELDS-IN-QUERY-FRAME-A 
&Scoped-define ENABLED-FIELDS-IN-QUERY-FRAME-A 
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-BROWSE-16}

/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_Arkiv 
       MENU-ITEM m_Avsluta      LABEL "Avsluta"       .

DEFINE SUB-MENU m_Uppdatera 
       MENU-ITEM m_Ny           LABEL "Ny"            
       MENU-ITEM m_ndra         LABEL "Ändra"         
       MENU-ITEM m_Bort         LABEL "Bort"          .

DEFINE MENU MENU-BAR-WINDOW-1 MENUBAR
       SUB-MENU  m_Arkiv        LABEL "Arkiv"         
       SUB-MENU  m_Uppdatera    LABEL "Uppdatera"     .


/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AND 
     LABEL "Ändra":L 
     SIZE 10 BY 1.18.

DEFINE BUTTON BTN_AVSL AUTO-END-KEY 
     LABEL "AVSLUTA":L 
     SIZE 10.5 BY 1.41.

DEFINE BUTTON BTN_BORT 
     LABEL "Bort":L 
     SIZE 10 BY 1.18.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 11 BY 1.18.

DEFINE VARIABLE FILL-IN_BEREDSKAPSAVTAL AS CHARACTER FORMAT "X(2)" INITIAL ? 
     LABEL "B-AVTAL" 
     VIEW-AS FILL-IN 
     SIZE 9.5 BY .91
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-47
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 13.5 BY 14.5
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 45 BY 13.73
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 41.5 BY 1.68
     BGCOLOR 8 .


/* Query definitions                                                    */
DEFINE QUERY BROWSE-16 FOR BEREDSKAPTAB SCROLLING.

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-16 QUERY BROWSE-16 NO-LOCK DISPLAY 
      BEREDSKAPTAB.BEREDSKAPSAVTAL
      BEREDSKAPTAB.BEREDSKAP
      BEREDSKAPTAB.DAGNR
      BEREDSKAPTAB.BERSTART1
      BEREDSKAPTAB.BERSTOPP1
      BEREDSKAPTAB.BERANTAL
    WITH SIZE 41.5 BY 8.5
         BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BTN_AVSL AT ROW 2.5 COL 50.5
     BROWSE-16 AT ROW 3 COL 6
     BTN_NY AT ROW 13 COL 6.13
     BTN_AND AT ROW 13 COL 21.13
     BTN_BORT AT ROW 13 COL 35.63
     FILL-IN_BEREDSKAPSAVTAL AT ROW 15.91 COL 17.88 COLON-ALIGNED
     RECT-6 AT ROW 1.5 COL 4
     RECT-47 AT ROW 1.5 COL 49
     "Beredskaptab" VIEW-AS TEXT
          SIZE 18 BY 1.09 AT ROW 11.64 COL 6
     RECT-7 AT ROW 12.68 COL 5.13
     "Sök på:" VIEW-AS TEXT
          SIZE 12 BY .68 AT ROW 15.41 COL 2.38
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 63.88 BY 16.

 

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Beredskaptab"
         COLUMN             = 13
         ROW                = 5.41
         HEIGHT             = 16.41
         WIDTH              = 65
         MAX-HEIGHT         = 20.18
         MAX-WIDTH          = 83.13
         VIRTUAL-HEIGHT     = 20.18
         VIRTUAL-WIDTH      = 83.13
         RESIZE             = yes
         SCROLL-BARS        = yes
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-WINDOW-1:HANDLE.
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WINDOW-1
  VISIBLE,,RUN-PERSISTENT                                               */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-16
/* Query rebuild information for BROWSE BROWSE-16
     _TblList          = "RT8.BEREDSKAPTAB"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = ""
     _FldNameList[1]   = RT8.BEREDSKAPTAB.BEREDSKAPSAVTAL
     _FldNameList[2]   = RT8.BEREDSKAPTAB.BEREDSKAP
     _FldNameList[3]   = RT8.BEREDSKAPTAB.DAGNR
     _FldNameList[4]   = RT8.BEREDSKAPTAB.BERSTART1
     _FldNameList[5]   = RT8.BEREDSKAPTAB.BERSTOPP1
     _FldNameList[6]   = RT8.BEREDSKAPTAB.BERANTAL
     _Query            is OPENED
*/  /* BROWSE BROWSE-16 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BROWSE-16
&Scoped-define SELF-NAME BROWSE-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-16 WINDOW-1
ON VALUE-CHANGED OF BROWSE-16 IN FRAME FRAME-A
DO:
  brec = RECID(BEREDSKAPTAB).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AND WINDOW-1
ON CHOOSE OF BTN_AND IN FRAME FRAME-A /* Ändra */
DO:
  RUN andra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVSL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVSL WINDOW-1
ON CHOOSE OF BTN_AVSL IN FRAME FRAME-A /* AVSLUTA */
DO:
  RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-1
ON CHOOSE OF BTN_BORT IN FRAME FRAME-A /* Bort */
DO:
  RUN bort.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WINDOW-1
ON CHOOSE OF BTN_NY IN FRAME FRAME-A /* Ny */
DO:
  RUN ny.
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_BEREDSKAPSAVTAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BEREDSKAPSAVTAL WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_BEREDSKAPSAVTAL IN FRAME FRAME-A /* B-AVTAL */
DO:
   DEFINE VARIABLE posok AS CHARACTER NO-UNDO.   
   posok = "".
   FILL-IN_BEREDSKAPSAVTAL = INPUT FILL-IN_BEREDSKAPSAVTAL.
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   ovrec = RECID(BEREDSKAPTAB).
   IF FILL-IN_BEREDSKAPSAVTAL = "" THEN DO:
      MESSAGE "SÖKBEGREPPET KAN INTE VARA BLANKT" VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_BEREDSKAPSAVTAL IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   posok = FILL-IN_BEREDSKAPSAVTAL.

   FIND BEREDSKAPTAB WHERE RECID(BEREDSKAPTAB) = ovrec NO-LOCK NO-ERROR.
   FIND NEXT BEREDSKAPTAB WHERE  BEREDSKAPTAB.BEREDSKAPSAVTAL = posok       
   USE-INDEX BERED NO-LOCK NO-ERROR.
   IF NOT AVAILABLE BEREDSKAPTAB THEN DO:
      MESSAGE "DET FINNS INGEN PÅ SÖKBEGREPPET" VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_BEREDSKAPSAVTAL IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   IF AVAILABLE BEREDSKAPTAB THEN DO:
      REPOSITION {&BROWSE-NAME} TO RECID RECID(BEREDSKAPTAB).
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Bort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Bort WINDOW-1
ON CHOOSE OF MENU-ITEM m_Bort /* Bort */
DO:
  RUN bort.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_ndra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_ndra WINDOW-1
ON CHOOSE OF MENU-ITEM m_ndra /* Ändra */
DO:
  RUN andra.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Ny WINDOW-1
ON CHOOSE OF MENU-ITEM m_Ny /* Ny */
DO:
  RUN ny.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.
   

/* These events will close the window and terminate the procedure.      */
/* (NOTE: this will override any user-defined triggers previously       */
/*  defined on the window.)                                             */
ON WINDOW-CLOSE OF {&WINDOW-NAME} DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.
ON ENDKEY, END-ERROR OF {&WINDOW-NAME} ANYWHERE DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  APPLY "HOME" TO {&BROWSE-NAME}.
  status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
  brec = RECID(BEREDSKAPTAB).
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra WINDOW-1 
PROCEDURE andra :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  {muswait.i}
  RUN BERETNY.W (INPUT RECID(BEREDSKAPTAB)). 
  {musarrow.i}
  IF musz = FALSE THEN DO: 
    OPEN QUERY {&BROWSE-NAME} FOR EACH BEREDSKAPTAB USE-INDEX BERED NO-LOCK.
    FIND FIRST BEREDSKAPTAB WHERE RECID(BEREDSKAPTAB) = brec NO-LOCK NO-ERROR. 
    IF AVAILABLE BEREDSKAPTAB THEN DO:
      REPOSITION {&BROWSE-NAME} TO RECID brec.
    END.
  END.
  musz = FALSE.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bort WINDOW-1 
PROCEDURE bort :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
MESSAGE "VILL DU VERKLIGEN TA BORT DENNA BEREDSKAPTAB?"
  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE BEREDSKAPTAB.BEREDSKAPSAVTAL
  UPDATE answer AS LOGICAL.
  IF answer THEN DO TRANSACTION:
    {muswait.i}
    FIND BEREDSKAPTAB WHERE RECID(BEREDSKAPTAB) = brec NO-LOCK NO-ERROR.
    FIND NEXT BEREDSKAPTAB NO-LOCK NO-ERROR.
    IF AVAILABLE BEREDSKAPTAB THEN borec = RECID(BEREDSKAPTAB).
    ELSE DO:
      FIND BEREDSKAPTAB WHERE RECID(BEREDSKAPTAB) = brec NO-LOCK NO-ERROR.
      FIND PREV BEREDSKAPTAB NO-LOCK NO-ERROR.
      IF AVAILABLE BEREDSKAPTAB THEN borec = RECID(BEREDSKAPTAB).
    END.
    FIND BEREDSKAPTAB WHERE RECID(BEREDSKAPTAB) = brec EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE BEREDSKAPTAB THEN DELETE BEREDSKAPTAB.
    OPEN QUERY {&BROWSE-NAME} FOR EACH BEREDSKAPTAB USE-INDEX BERED NO-LOCK. 
    brec = borec.
    FIND FIRST BEREDSKAPTAB WHERE RECID(BEREDSKAPTAB) = brec NO-LOCK NO-ERROR.  
    IF AVAILABLE BEREDSKAPTAB THEN DO:
      REPOSITION {&BROWSE-NAME} TO RECID brec.
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}. 
    END.  
    {musarrow.i}   
  END. 
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-1 _DEFAULT-DISABLE
PROCEDURE disable_UI :
/* --------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
   -------------------------------------------------------------------- */
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U THEN DELETE WIDGET WINDOW-1.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-1 _DEFAULT-ENABLE
PROCEDURE enable_UI :
/* --------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
   -------------------------------------------------------------------- */
  DISPLAY FILL-IN_BEREDSKAPSAVTAL 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE RECT-6 RECT-47 BTN_AVSL BROWSE-16 RECT-7 BTN_NY BTN_AND BTN_BORT 
         FILL-IN_BEREDSKAPSAVTAL 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW WINDOW-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ny WINDOW-1 
PROCEDURE ny :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  {muswait.i}
  RUN BERETNY.W (INPUT ?).
  {musarrow.i}
   IF musz = FALSE THEN DO: 
    OPEN QUERY {&BROWSE-NAME} FOR EACH BEREDSKAPTAB USE-INDEX BERED NO-LOCK.
    FIND FIRST BEREDSKAPTAB WHERE RECID(BEREDSKAPTAB) = brec NO-LOCK NO-ERROR. 
    IF AVAILABLE BEREDSKAPTAB THEN DO:
      REPOSITION {&BROWSE-NAME} TO RECID brec.
    END.
  END.
  musz = FALSE.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE BROWSE-NAME
&UNDEFINE FRAME-NAME
&UNDEFINE WINDOW-NAME
