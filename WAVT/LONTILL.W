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
DEFINE NEW SHARED VARIABLE ovrec AS RECID NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE rkod LIKE TIDREGITAB.LONTILLAGG NO-UNDO.  
DEFINE VARIABLE kod LIKE TIDREGITAB.LONTILLAGG NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_TILL

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES LONTILL

/* Definitions for BROWSE BRW_TILL                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_TILL LONTILL.LONTILLAGG LONTILL.KOD ~
LONTILL.VILART LONTILL.LONKODTEXT LONTILL.ENHET LONTILL.ERSATTNING ~
LONTILL.TYPKOD LONTILL.VALBAR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TILL LONTILL.LONTILLAGG 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_TILL LONTILL
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_TILL LONTILL
&Scoped-define QUERY-STRING-BRW_TILL FOR EACH LONTILL NO-LOCK ~
    BY LONTILL.KOD ~
       BY LONTILL.VILART INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_TILL OPEN QUERY BRW_TILL FOR EACH LONTILL NO-LOCK ~
    BY LONTILL.KOD ~
       BY LONTILL.VILART INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_TILL LONTILL
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TILL LONTILL


/* Definitions for FRAME FRAME-A                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-BRW_TILL}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_TILL BTN_NY BTN_AND BTN_BORT BTN_AVSL ~
FILL-IN_LONTILLAGG FILL-IN_VILART 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_LONTILLAGG FILL-IN_VILART 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AND 
     LABEL "Ändra":L 
     SIZE 10 BY 1.

DEFINE BUTTON BTN_AVSL AUTO-END-KEY 
     LABEL "AVSLUTA":L 
     SIZE 10.5 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Bort":L 
     SIZE 10 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 11 BY 1.

DEFINE VARIABLE FILL-IN_LONTILLAGG AS CHARACTER FORMAT "X(5)" INITIAL ? 
     LABEL "LONTILLAGG" 
     VIEW-AS FILL-IN 
     SIZE 9.5 BY .92.

DEFINE VARIABLE FILL-IN_VILART AS CHARACTER FORMAT "X(5)" INITIAL ? 
     LABEL "VISAD LART" 
     VIEW-AS FILL-IN 
     SIZE 9.5 BY .92.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_TILL FOR 
      LONTILL SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_TILL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TILL WINDOW-1 _STRUCTURED
  QUERY BRW_TILL NO-LOCK DISPLAY
      LONTILL.LONTILLAGG COLUMN-LABEL "LÖNEART" FORMAT "x(5)":U
      LONTILL.KOD FORMAT "x(2)":U
      LONTILL.VILART COLUMN-LABEL "VISAD LÖNEART" FORMAT "X(8)":U
      LONTILL.LONKODTEXT FORMAT "x(40)":U
      LONTILL.ENHET FORMAT "x(2)":U
      LONTILL.ERSATTNING COLUMN-LABEL "ERSÄTT" FORMAT "->>>9.99":U
      LONTILL.TYPKOD FORMAT "X(3)":U
      LONTILL.VALBAR FORMAT "yes/no":U
  ENABLE
      LONTILL.LONTILLAGG
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 97 BY 20.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_TILL AT ROW 2.5 COL 2.5
     BTN_NY AT ROW 24.46 COL 7
     BTN_AND AT ROW 24.46 COL 22
     BTN_BORT AT ROW 24.46 COL 36.5
     BTN_AVSL AT ROW 27 COL 86.25
     FILL-IN_LONTILLAGG AT ROW 27.04 COL 18.63 COLON-ALIGNED
     FILL-IN_VILART AT ROW 27.04 COL 41.13 COLON-ALIGNED
     "Sök på:" VIEW-AS TEXT
          SIZE 8.5 BY .67 AT ROW 26.25 COL 2.5
     "Lontill" VIEW-AS TEXT
          SIZE 18 BY .75 AT ROW 23.25 COL 7.13
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 100 BY 28.


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
         TITLE              = "Lontill"
         COLUMN             = 14.63
         ROW                = 7.79
         HEIGHT             = 28.08
         WIDTH              = 100
         MAX-HEIGHT         = 28.08
         MAX-WIDTH          = 102.38
         VIRTUAL-HEIGHT     = 28.08
         VIRTUAL-WIDTH      = 102.38
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
/* SETTINGS FOR WINDOW WINDOW-1
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_TILL TEXT-2 FRAME-A */
ASSIGN 
       BRW_TILL:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE
       BRW_TILL:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TILL
/* Query rebuild information for BROWSE BRW_TILL
     _TblList          = "rt9.LONTILL"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "rt9.LONTILL.KOD|yes,rt9.LONTILL.VILART|yes"
     _FldNameList[1]   > rt9.LONTILL.LONTILLAGG
"LONTILL.LONTILLAGG" "LÖNEART" "x(5)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = rt9.LONTILL.KOD
     _FldNameList[3]   > rt9.LONTILL.VILART
"LONTILL.VILART" "VISAD LÖNEART" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > rt9.LONTILL.LONKODTEXT
"LONTILL.LONKODTEXT" ? "x(40)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = rt9.LONTILL.ENHET
     _FldNameList[6]   > rt9.LONTILL.ERSATTNING
"LONTILL.ERSATTNING" "ERSÄTT" "->>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   = rt9.LONTILL.TYPKOD
     _FldNameList[8]   = rt9.LONTILL.VALBAR
     _Query            is OPENED
*/  /* BROWSE BRW_TILL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_TILL
&Scoped-define SELF-NAME BRW_TILL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_TILL WINDOW-1
ON VALUE-CHANGED OF BRW_TILL IN FRAME FRAME-A
DO:
  brec = RECID(LONTILL).
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


&Scoped-define SELF-NAME FILL-IN_LONTILLAGG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_LONTILLAGG WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_LONTILLAGG IN FRAME FRAME-A /* LONTILLAGG */
DO:
   DEFINE VARIABLE posok AS CHARACTER NO-UNDO.   
   posok = "".
   FILL-IN_LONTILLAGG = INPUT FILL-IN_LONTILLAGG.
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   ovrec = RECID(LONTILL).
   IF FILL-IN_LONTILLAGG = "" THEN DO:
      MESSAGE "SÖKBEGREPPET KAN INTE VARA BLANKT" VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_LONTILLAGG IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   posok = FILL-IN_LONTILLAGG.

   FIND LONTILL WHERE RECID(LONTILL) = ovrec NO-LOCK NO-ERROR.
   FIND NEXT LONTILL WHERE  LONTILL.LONTILLAGG = posok       
   USE-INDEX LON NO-LOCK NO-ERROR.
   IF NOT AVAILABLE LONTILL THEN DO:
      FIND FIRST LONTILL WHERE  LONTILL.LONTILLAGG = posok       
      USE-INDEX LON NO-LOCK NO-ERROR.
      IF NOT AVAILABLE LONTILL THEN DO:
         MESSAGE "DET FINNS INGEN PÅ SÖKBEGREPPET" VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN_LONTILLAGG IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
   END.
   IF AVAILABLE LONTILL THEN DO:
      REPOSITION {&BROWSE-NAME} TO RECID RECID(LONTILL).
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_VILART
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_VILART WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_VILART IN FRAME FRAME-A /* VISAD LART */
DO:
   DEFINE VARIABLE posok AS CHARACTER NO-UNDO.   
   posok = "".
   FILL-IN_VILART = INPUT FILL-IN_VILART.
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   ovrec = RECID(LONTILL).
   IF FILL-IN_VILART = "" THEN DO:
      MESSAGE "SÖKBEGREPPET KAN INTE VARA BLANKT" VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_VILART IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   posok = FILL-IN_VILART.

   FIND LONTILL WHERE RECID(LONTILL) = ovrec NO-LOCK NO-ERROR.
   FIND NEXT LONTILL WHERE  LONTILL.VILART = posok       
   USE-INDEX VILART NO-LOCK NO-ERROR.
   IF NOT AVAILABLE LONTILL THEN DO:
      FIND FIRST LONTILL WHERE  LONTILL.VILART = posok       
      USE-INDEX VILART NO-LOCK NO-ERROR.
      IF NOT AVAILABLE LONTILL THEN DO:
         MESSAGE "DET FINNS INGEN PÅ SÖKBEGREPPET" VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN_VILART IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
   END.
   IF AVAILABLE LONTILL THEN DO:
      REPOSITION {&BROWSE-NAME} TO RECID RECID(LONTILL).
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
   END.   
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
  /*{ALLSTARTDYN.I}*/
  RUN enable_UI.
  APPLY "HOME" TO {&BROWSE-NAME}.
  status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
  brec = RECID(LONTILL).
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
  RUN LONTINY.W (INPUT RECID(LONTILL)). 
  {musarrow.i}
  IF musz = FALSE THEN DO: 
    OPEN QUERY {&BROWSE-NAME} FOR EACH LONTILL USE-INDEX VILART NO-LOCK INDEXED-REPOSITION.
    /*RUN openbdynspec_UI IN brwproc[1].            */
    FIND FIRST LONTILL WHERE RECID(LONTILL) = brec NO-LOCK NO-ERROR. 
    IF AVAILABLE LONTILL THEN DO:
      REPOSITION {&BROWSE-NAME} TO RECID brec.
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
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
MESSAGE "VILL DU VERKLIGEN TA BORT DENNA LONTILL?"
  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE LONTILL.LONTILLAGG
  UPDATE answer AS LOGICAL.
  IF answer THEN DO TRANSACTION:
    {muswait.i}
    brec = RECID(LONTILL).
    FIND LONTILL WHERE RECID(LONTILL) = brec NO-LOCK NO-ERROR.
    kod = LONTILL.LONTILLAGG.
    FIND NEXT LONTILL USE-INDEX VILART NO-LOCK NO-ERROR.
    IF AVAILABLE LONTILL THEN borec = RECID(LONTILL).
    ELSE DO:
      FIND LONTILL WHERE RECID(LONTILL) = brec NO-LOCK NO-ERROR.
      FIND PREV LONTILL USE-INDEX VILART NO-LOCK NO-ERROR.
      IF AVAILABLE LONTILL THEN borec = RECID(LONTILL).
    END.
    FIND LONTILL WHERE RECID(LONTILL) = brec EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE LONTILL THEN DELETE LONTILL.
    FIND FIRST LONTILL WHERE LONTILL.LONTILLAGG = kod USE-INDEX LON NO-LOCK NO-ERROR.
    IF NOT AVAILABLE LONTILL THEN DO:
       FIND FIRST RESTAB WHERE RESTAB.RESKOD = kod USE-INDEX RESTAB EXCLUSIVE-LOCK
       NO-ERROR.
       IF AVAILABLE RESTAB THEN DELETE RESTAB.
       FIND FIRST LONETAB WHERE LONETAB.LONTILLAGG = kod USE-INDEX LONETAB EXCLUSIVE-LOCK
       NO-ERROR.
       IF AVAILABLE LONETAB THEN DELETE LONETAB.
    END.  
    OPEN QUERY {&BROWSE-NAME} FOR EACH LONTILL USE-INDEX VILART NO-LOCK INDEXED-REPOSITION. 
    /*RUN openbdynspec_UI IN brwproc[1].            */
    brec = borec.
    FIND FIRST LONTILL WHERE RECID(LONTILL) = brec NO-LOCK NO-ERROR.  
    IF AVAILABLE LONTILL THEN DO:
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
  DISPLAY FILL-IN_LONTILLAGG FILL-IN_VILART 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE BRW_TILL BTN_NY BTN_AND BTN_BORT BTN_AVSL FILL-IN_LONTILLAGG 
         FILL-IN_VILART 
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
  RUN LONTINY.W (INPUT ?).
  {musarrow.i}
   IF musz = FALSE THEN DO: 
    OPEN QUERY {&BROWSE-NAME} FOR EACH LONTILL USE-INDEX VILART NO-LOCK INDEXED-REPOSITION.
    /*RUN openbdynspec_UI IN brwproc[1].            */
    FIND FIRST LONTILL WHERE RECID(LONTILL) = brec NO-LOCK NO-ERROR. 
    IF AVAILABLE LONTILL THEN DO:
      REPOSITION {&BROWSE-NAME} TO RECID brec.
    END.
  END.
  musz = FALSE.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

