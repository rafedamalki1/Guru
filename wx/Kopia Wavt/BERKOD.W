&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          rt9              PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-1
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
{ALLDEF.I}
{GLOBVAR2DEL1.I}
DEFINE NEW SHARED VARIABLE brec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE borec AS RECID NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_BER

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES BERKOD

/* Definitions for BROWSE BRW_BER                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_BER BERKOD.BEREDSKAP ~
BERKOD.BEREDSKAPSAVTAL BERKOD.VILART BERKOD.LONKODTEXT BERKOD.ENHET ~
BERKOD.ERSATTNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BER BERKOD.BEREDSKAP 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_BER BERKOD
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_BER BERKOD
&Scoped-define QUERY-STRING-BRW_BER FOR EACH BERKOD NO-LOCK ~
    BY BERKOD.BEREDSKAPSAVTAL ~
       BY BERKOD.VILART
&Scoped-define OPEN-QUERY-BRW_BER OPEN QUERY BRW_BER FOR EACH BERKOD NO-LOCK ~
    BY BERKOD.BEREDSKAPSAVTAL ~
       BY BERKOD.VILART.
&Scoped-define TABLES-IN-QUERY-BRW_BER BERKOD
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BER BERKOD


/* Definitions for FRAME FRAME-A                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-BRW_BER}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-6 RECT-47 RECT-7 BRW_BER BTN_AVSL ~
BTN_NY BTN_AND BTN_BORT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



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
     SIZE 10 BY 1.17.

DEFINE BUTTON BTN_AVSL AUTO-END-KEY 
     LABEL "AVSLUTA":L 
     SIZE 10.5 BY 1.42.

DEFINE BUTTON BTN_BORT 
     LABEL "Bort":L 
     SIZE 10 BY 1.17.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 11 BY 1.17.

DEFINE RECTANGLE RECT-47
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 13.5 BY 14.5
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 82.5 BY 14.5
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 41.5 BY 2.17
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_BER FOR 
      BERKOD SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_BER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BER WINDOW-1 _STRUCTURED
  QUERY BRW_BER NO-LOCK DISPLAY
      BERKOD.BEREDSKAP COLUMN-LABEL "Berkod" FORMAT "x(4)":U
      BERKOD.BEREDSKAPSAVTAL COLUMN-LABEL "Beredskaps!avtal" FORMAT "XX":U
      BERKOD.VILART COLUMN-LABEL "Visad! berkod" FORMAT "X(8)":U
      BERKOD.LONKODTEXT COLUMN-LABEL "Text" FORMAT "X(25)":U
      BERKOD.ENHET COLUMN-LABEL "Enhet" FORMAT "x(2)":U
      BERKOD.ERSATTNING COLUMN-LABEL "Ersättning kr" FORMAT "->>>>9.99":U
  ENABLE
      BERKOD.BEREDSKAP
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 78.88 BY 8.5
         BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_BER AT ROW 2.5 COL 2.63
     BTN_AVSL AT ROW 2.75 COL 85.5
     BTN_NY AT ROW 14 COL 6.5
     BTN_AND AT ROW 14 COL 21.5
     BTN_BORT AT ROW 14 COL 36
     "Berkod" VIEW-AS TEXT
          SIZE 18 BY 1.5 AT ROW 12 COL 6.5
     RECT-6 AT ROW 1.5 COL 1.5
     RECT-47 AT ROW 1.5 COL 84
     RECT-7 AT ROW 13.5 COL 5.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 96.88 BY 15.54.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Berkod"
         COLUMN             = 9
         ROW                = 5.42
         HEIGHT             = 15.83
         WIDTH              = 97.38
         MAX-HEIGHT         = 20.17
         MAX-WIDTH          = 97.38
         VIRTUAL-HEIGHT     = 20.17
         VIRTUAL-WIDTH      = 97.38
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WINDOW-1
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
/* BROWSE-TAB BRW_BER RECT-7 FRAME-A */
ASSIGN 
       BRW_BER:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE
       BRW_BER:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BER
/* Query rebuild information for BROWSE BRW_BER
     _TblList          = "rt9.BERKOD"
     _Options          = "NO-LOCK"
     _OrdList          = "rt9.BERKOD.BEREDSKAPSAVTAL|yes,rt9.BERKOD.VILART|yes"
     _FldNameList[1]   > rt9.BERKOD.BEREDSKAP
"BEREDSKAP" "Berkod" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > rt9.BERKOD.BEREDSKAPSAVTAL
"BEREDSKAPSAVTAL" "Beredskaps!avtal" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > rt9.BERKOD.VILART
"VILART" "Visad! berkod" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > rt9.BERKOD.LONKODTEXT
"LONKODTEXT" "Text" "X(25)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > rt9.BERKOD.ENHET
"ENHET" "Enhet" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > rt9.BERKOD.ERSATTNING
"ERSATTNING" "Ersättning kr" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_BER */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_BER
&Scoped-define SELF-NAME BRW_BER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_BER WINDOW-1
ON VALUE-CHANGED OF BRW_BER IN FRAME FRAME-A
DO:
  status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.

  brec = RECID(BERKOD).  
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
  {ALLSTARTDYN.I}
  RUN enable_UI.
  APPLY "HOME" TO {&BROWSE-NAME}.
  status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
  brec = RECID(BERKOD).
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_BER:HANDLE IN FRAME {&FRAME-NAME}).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra WINDOW-1 
PROCEDURE andra :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  {muswait.i}
  RUN BKODNY.W (INPUT RECID(BERKOD)). 
  {musarrow.i}
  IF musz = FALSE THEN DO: 
    /*OPEN QUERY {&BROWSE-NAME} FOR EACH BERKOD USE-INDEX VILART NO-LOCK.*/
    RUN openbdynspec_UI IN brwproc[1].            
    FIND FIRST BERKOD WHERE RECID(BERKOD) = brec NO-LOCK NO-ERROR. 
    IF AVAILABLE BERKOD THEN DO:
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
MESSAGE "VILL DU VERKLIGEN TA BORT DENNA BERKOD?"
  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE BERKOD.BEREDSKAPSAVTAL
  UPDATE answer AS LOGICAL.
  IF answer THEN DO TRANSACTION:
    {muswait.i}
    FIND BERKOD WHERE RECID(BERKOD) = brec NO-LOCK NO-ERROR.
    FIND NEXT BERKOD NO-LOCK NO-ERROR.
    IF AVAILABLE BERKOD THEN borec = RECID(BERKOD).
    ELSE DO:
      FIND BERKOD WHERE RECID(BERKOD) = brec NO-LOCK NO-ERROR.
      FIND PREV BERKOD NO-LOCK NO-ERROR.
      IF AVAILABLE BERKOD THEN borec = RECID(BERKOD).
    END.
    FIND BERKOD WHERE RECID(BERKOD) = brec EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE BERKOD THEN DELETE BERKOD.
    /*OPEN QUERY {&BROWSE-NAME} FOR EACH BERKOD USE-INDEX VILART NO-LOCK. */
    RUN openbdynspec_UI IN brwproc[1].            
    brec = borec.
    FIND FIRST BERKOD WHERE RECID(BERKOD) = brec NO-LOCK NO-ERROR.  
    IF AVAILABLE BERKOD THEN DO:
      REPOSITION {&BROWSE-NAME} TO RECID brec.
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}. 
    END.  
    {musarrow.i}   
  END. 
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-1  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
  THEN DELETE WIDGET WINDOW-1.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-1  _DEFAULT-ENABLE
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
  ENABLE RECT-6 RECT-47 RECT-7 BRW_BER BTN_AVSL BTN_NY BTN_AND BTN_BORT 
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
  RUN BKODNY.W (INPUT ?).
  {musarrow.i}
   IF musz = FALSE THEN DO: 
    /*OPEN QUERY {&BROWSE-NAME} FOR EACH BERKOD USE-INDEX VILART NO-LOCK.*/
    RUN openbdynspec_UI IN brwproc[1].            
    FIND FIRST BERKOD WHERE RECID(BERKOD) = brec NO-LOCK NO-ERROR. 
    IF AVAILABLE BERKOD THEN DO:
      REPOSITION {&BROWSE-NAME} TO RECID brec.
    END.
  END.
  musz = FALSE.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

