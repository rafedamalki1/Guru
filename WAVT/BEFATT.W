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

DEFINE NEW SHARED VARIABLE brec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE borec AS RECID NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE morgon LIKE ARBETSTIDTAB.START NO-UNDO.
DEFINE VARIABLE kvall LIKE ARBETSTIDTAB.SLUT NO-UNDO.
DEFINE QUERY bestq FOR BESTTAB.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BROWSE-16

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES BEFATTNINGSTAB

/* Definitions for BROWSE BROWSE-16                                     */
&Scoped-define FIELDS-IN-QUERY-BROWSE-16 BEFATTNINGSTAB.BEFATTNING ~
BEFATTNINGSTAB.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-16 
&Scoped-define QUERY-STRING-BROWSE-16 FOR EACH BEFATTNINGSTAB NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-16 OPEN QUERY BROWSE-16 FOR EACH BEFATTNINGSTAB NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-16 BEFATTNINGSTAB
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-16 BEFATTNINGSTAB


/* Definitions for FRAME FRAME-A                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-BROWSE-16}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-16 BTN_NY BTN_AND BTN_BORT BTN_AVSL 

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
     SIZE 10 BY 1.17.

DEFINE BUTTON BTN_AVSL AUTO-END-KEY 
     LABEL "AVSLUTA":L 
     SIZE 10.5 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Bort":L 
     SIZE 10 BY 1.17.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 11 BY 1.17.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-16 FOR 
      BEFATTNINGSTAB SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-16 WINDOW-1 _STRUCTURED
  QUERY BROWSE-16 NO-LOCK DISPLAY
      BEFATTNINGSTAB.BEFATTNING FORMAT "x(20)":U
      BEFATTNINGSTAB.NAMN COLUMN-LABEL "Visa!Befattning" FORMAT "x(20)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 54.5 BY 8.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BROWSE-16 AT ROW 3 COL 7.5
     BTN_NY AT ROW 14 COL 6.5
     BTN_AND AT ROW 14 COL 21.5
     BTN_BORT AT ROW 14 COL 36
     BTN_AVSL AT ROW 15.25 COL 54
     "Befattningstab" VIEW-AS TEXT
          SIZE 18 BY 1.5 AT ROW 12 COL 6.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 63.88 BY 16.


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
         TITLE              = "Befattningstab"
         COLUMN             = 15.88
         ROW                = 5.04
         HEIGHT             = 16.42
         WIDTH              = 65
         MAX-HEIGHT         = 20.17
         MAX-WIDTH          = 83.13
         VIRTUAL-HEIGHT     = 20.17
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WINDOW-1
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-16 TEXT-1 FRAME-A */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-16
/* Query rebuild information for BROWSE BROWSE-16
     _TblList          = "RT9.BEFATTNINGSTAB"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = RT9.BEFATTNINGSTAB.BEFATTNING
     _FldNameList[2]   > RT9.BEFATTNINGSTAB.NAMN
"BEFATTNINGSTAB.NAMN" "Visa!Befattning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-16 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BROWSE-16
&Scoped-define SELF-NAME BROWSE-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-16 WINDOW-1
ON VALUE-CHANGED OF BROWSE-16 IN FRAME FRAME-A
DO:
  brec = RECID(BEFATTNINGSTAB).
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
  brec = RECID(BEFATTNINGSTAB).
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
   RUN BEFATTNY.W (INPUT RECID(BEFATTNINGSTAB)). 
   {musarrow.i}
   IF musz = FALSE THEN DO: 
      RUN skapatab_UI.       
      OPEN QUERY {&BROWSE-NAME} FOR EACH BEFATTNINGSTAB NO-LOCK.
      FIND FIRST BEFATTNINGSTAB WHERE RECID(BEFATTNINGSTAB) = brec NO-LOCK NO-ERROR. 
      IF AVAILABLE BEFATTNINGSTAB THEN DO:
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
   MESSAGE "VILL DU VERKLIGEN TA BORT DENNA BEFATTNINGSTAB?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE BEFATTNINGSTAB.BEFATTNING
   UPDATE answer AS LOGICAL.
   IF answer THEN DO TRANSACTION:
      {muswait.i}
      FIND BEFATTNINGSTAB WHERE RECID(BEFATTNINGSTAB) = brec NO-LOCK NO-ERROR.
      FIND NEXT BEFATTNINGSTAB NO-LOCK NO-ERROR.
      IF AVAILABLE BEFATTNINGSTAB THEN borec = RECID(BEFATTNINGSTAB).
      ELSE DO:
         FIND BEFATTNINGSTAB WHERE RECID(BEFATTNINGSTAB) = brec NO-LOCK NO-ERROR.
         FIND PREV BEFATTNINGSTAB NO-LOCK NO-ERROR.
         IF AVAILABLE BEFATTNINGSTAB THEN borec = RECID(BEFATTNINGSTAB).
      END.
      RUN borttab_UI.      
      OPEN QUERY {&BROWSE-NAME} FOR EACH BEFATTNINGSTAB NO-LOCK. 
      brec = borec.
      FIND FIRST BEFATTNINGSTAB WHERE RECID(BEFATTNINGSTAB) = brec NO-LOCK NO-ERROR.  
      IF AVAILABLE BEFATTNINGSTAB THEN DO:
         REPOSITION {&BROWSE-NAME} TO RECID brec.
         status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}. 
      END.  
      {musarrow.i}   
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE borttab_UI WINDOW-1 
PROCEDURE borttab_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND BEFATTNINGSTAB WHERE RECID(BEFATTNINGSTAB) = brec EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE BEFATTNINGSTAB THEN DO:
      OPEN QUERY bestq FOR EACH BESTTAB USE-INDEX BEST NO-LOCK.
      GET FIRST bestq NO-LOCK.
      DO WHILE AVAILABLE(BESTTAB):         
         FIND FIRST KUNDBEF WHERE KUNDBEF.BESTID = BESTTAB.BESTID AND
         KUNDBEF.BEFATTNING = BEFATTNINGSTAB.BEFATTNING EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE KUNDBEF THEN DO:
            DELETE KUNDBEF.            
         END.
         FOR EACH KUNDOVER WHERE KUNDOVER.BESTID = BESTTAB.BESTID AND
         KUNDOVER.BEFATTNING = BEFATTNINGSTAB.BEFATTNING
         USE-INDEX KUNDOVER EXCLUSIVE-LOCK:
            DELETE KUNDOVER.
         END.
         FOR EACH PRISLISTFAKT WHERE 
         PRISLISTFAKT.BEFATTNING = BEFATTNINGSTAB.BEFATTNING
         EXCLUSIVE-LOCK:
            DELETE PRISLISTFAKT.
         END.
         GET NEXT bestq NO-LOCK.         
      END.
      DELETE BEFATTNINGSTAB.   
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
  ENABLE BROWSE-16 BTN_NY BTN_AND BTN_BORT BTN_AVSL 
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
   RUN BEFATTNY.W (INPUT ?).
   {musarrow.i}
   IF musz = FALSE THEN DO: 
      RUN skapatab_UI.
      OPEN QUERY {&BROWSE-NAME} FOR EACH BEFATTNINGSTAB NO-LOCK.
      FIND FIRST BEFATTNINGSTAB WHERE RECID(BEFATTNINGSTAB) = brec NO-LOCK NO-ERROR. 
      IF AVAILABLE BEFATTNINGSTAB THEN DO:
         REPOSITION {&BROWSE-NAME} TO RECID brec.
      END.
   END.
   musz = FALSE.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skapatab_UI WINDOW-1 
PROCEDURE skapatab_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FIND FIRST BEFATTNINGSTAB WHERE RECID(BEFATTNINGSTAB) = brec NO-LOCK NO-ERROR.                  
   OPEN QUERY bestq FOR EACH BESTTAB USE-INDEX BEST NO-LOCK.
   GET FIRST bestq NO-LOCK.
   DO WHILE AVAILABLE(BESTTAB):         
      FIND FIRST KUNDBEF WHERE KUNDBEF.BESTID = BESTTAB.BESTID AND
      KUNDBEF.BEFATTNING = BEFATTNINGSTAB.BEFATTNING USE-INDEX KUNDBEF NO-LOCK NO-ERROR.
      IF NOT AVAILABLE KUNDBEF THEN DO TRANSACTION:
         CREATE KUNDBEF.
         ASSIGN 
         KUNDBEF.BESTID = BESTTAB.BESTID 
         KUNDBEF.BEFATTNING = BEFATTNINGSTAB.BEFATTNING
         KUNDBEF.PRISA = BEFATTNINGSTAB.PRISA.         
      END. 
      FIND FIRST KUNDOVER WHERE KUNDOVER.BESTID = BESTTAB.BESTID AND
      KUNDOVER.BEFATTNING = BEFATTNINGSTAB.BEFATTNING
      USE-INDEX KUNDOVER NO-LOCK NO-ERROR.
      IF NOT AVAILABLE KUNDOVER THEN DO TRANSACTION:
         FIND FIRST PERSONALTAB WHERE PERSONALTAB.BEFATTNING = BEFATTNINGSTAB.BEFATTNING
         USE-INDEX PBEF NO-LOCK NO-ERROR.
         IF NOT AVAILABLE PERSONALTAB THEN DO:
            ASSIGN
            morgon = 7.00
            kvall = 16.00.
         END.
         ELSE DO:
            FIND FIRST VECKOARBETID WHERE VECKOARBETID.VECKOSCHEMA = PERSONALTAB.VECKOSCHEMA
            USE-INDEX VECKOSCHEMA NO-LOCK NO-ERROR.
            FIND FIRST ARBETSTIDTAB WHERE ARBETSTIDTAB.ARBTIDKOD = VECKOARBETID.ARBTIDMAN
            USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.
            ASSIGN
            morgon = ARBETSTIDTAB.START
            kvall = ARBETSTIDTAB.SLUT.
         END.
         CREATE KUNDOVER.
         ASSIGN 
         KUNDOVER.BEFATTNING = BEFATTNINGSTAB.BEFATTNING
         KUNDOVER.BESTID = BESTTAB.BESTID 
         KUNDOVER.DAGTYP = "VARDAGAR"
         KUNDOVER.EQDAG = 2 
         KUNDOVER.PRISA = KUNDBEF.PRISA
         KUNDOVER.START = 00.00
         KUNDOVER.SLUT = morgon.
         CREATE KUNDOVER.
         ASSIGN 
         KUNDOVER.BEFATTNING = BEFATTNINGSTAB.BEFATTNING
         KUNDOVER.BESTID = BESTTAB.BESTID 
         KUNDOVER.DAGTYP = "VARDAGAR"
         KUNDOVER.EQDAG = 2 
         KUNDOVER.PRISA = KUNDBEF.PRISA
         KUNDOVER.START = kvall
         KUNDOVER.SLUT = 24.00.        
         CREATE KUNDOVER.
         ASSIGN 
         KUNDOVER.BEFATTNING = BEFATTNINGSTAB.BEFATTNING
         KUNDOVER.BESTID = BESTTAB.BESTID 
         KUNDOVER.DAGTYP = "LÖRDAGAR"
         KUNDOVER.EQDAG = 7 
         KUNDOVER.PRISA = KUNDBEF.PRISA
         KUNDOVER.START = 00.00
         KUNDOVER.SLUT = 24.00.         
         CREATE KUNDOVER.
         ASSIGN 
         KUNDOVER.BEFATTNING = BEFATTNINGSTAB.BEFATTNING
         KUNDOVER.BESTID = BESTTAB.BESTID 
         KUNDOVER.DAGTYP = "SÖNDAGAR"
         KUNDOVER.EQDAG = 8
         KUNDOVER.PRISA = KUNDBEF.PRISA
         KUNDOVER.START = 00.00
         KUNDOVER.SLUT = 24.00.
      END.        
      GET NEXT bestq NO-LOCK.
   END.
   CLOSE QUERY bestq. 
   OPEN QUERY dq FOR EACH DEFPRISLISTA NO-LOCK.
   GET FIRST dq NO-LOCK.
   DO WHILE AVAILABLE(DEFPRISLISTA):
      IF DEFPRISLISTA.OVER = TRUE THEN DO:
         FIND FIRST OVERPRISLISTA WHERE OVERPRISLISTA.PRISID = DEFPRISLISTA.PRISID AND
         OVERPRISLISTA.BEFATTNING = BEFATTNING.BEFATTNING NO-LOCK NO-ERROR.       
         IF NOT AVAILABLE OVERPRISLISTA THEN DO TRANSACTION:
            ASSIGN
            morgon = 7.00
            kvall = 16.00.
            FIND FIRST OVERTEXTFAKT WHERE OVERTEXTFAKT.BEFATTNING = BEFATTNINGSTAB.BEFATTNING
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE OVERTEXTFAKT THEN DO:
               IF SETUSERID({setuser.I},{setpwd.I},"DICTDB") THEN DO:
                  CREATE OVERTEXTFAKT.
                  ASSIGN
                  OVERTEXTFAKT.BEFATTNING = BEFATTNINGSTAB.BEFATTNING
                  OVERTEXTFAKT.OTEXT = BEFATTNINGSTAB.NAMN
                  OVERTEXTFAKT.OTEXTID = STRING(NEXT-VALUE(PRISLISTSEQ)).
               END.
            END.
            CREATE OVERPRISLISTA.
            ASSIGN 
            OVERPRISLISTA.PRISID = DEFPRISLISTA.PRISID 
            OVERPRISLISTA.BEFATTNING = BEFATTNINGSTAB.BEFATTNING                  
            OVERPRISLISTA.OTEXTID = OVERTEXTFAKT.OTEXTID
            OVERPRISLISTA.DAGTYP = "VARDAGAR"
            OVERPRISLISTA.EQDAG = 2 
            OVERPRISLISTA.PRISA = BEFATTNINGSTAB.PRISA
            OVERPRISLISTA.START = 00.00
            OVERPRISLISTA.SLUT = morgon.
            CREATE OVERPRISLISTA.
            ASSIGN 
            OVERPRISLISTA.PRISID = DEFPRISLISTA.PRISID 
            OVERPRISLISTA.BEFATTNING = BEFATTNINGSTAB.BEFATTNING
            OVERPRISLISTA.OTEXTID = OVERTEXTFAKT.OTEXTID
            OVERPRISLISTA.DAGTYP = "VARDAGAR"
            OVERPRISLISTA.EQDAG = 2 
            OVERPRISLISTA.PRISA = BEFATTNINGSTAB.PRISA
            OVERPRISLISTA.START = kvall
            OVERPRISLISTA.SLUT = 24.00.        
            CREATE OVERPRISLISTA.
            ASSIGN 
            OVERPRISLISTA.PRISID = DEFPRISLISTA.PRISID 
            OVERPRISLISTA.BEFATTNING = BEFATTNINGSTAB.BEFATTNING
            OVERPRISLISTA.OTEXTID = OVERTEXTFAKT.OTEXTID         
            OVERPRISLISTA.DAGTYP = "LÖRDAGAR"
            OVERPRISLISTA.EQDAG = 7 
            OVERPRISLISTA.PRISA = BEFATTNINGSTAB.PRISA
            OVERPRISLISTA.START = 00.00
            OVERPRISLISTA.SLUT = 24.00.         
            CREATE OVERPRISLISTA.
            ASSIGN 
            OVERPRISLISTA.PRISID = DEFPRISLISTA.PRISID 
            OVERPRISLISTA.BEFATTNING = BEFATTNINGSTAB.BEFATTNING
            OVERPRISLISTA.OTEXTID = OVERTEXTFAKT.OTEXTID
            OVERPRISLISTA.DAGTYP = "SÖNDAGAR"
            OVERPRISLISTA.EQDAG = 8
            OVERPRISLISTA.PRISA = BEFATTNINGSTAB.PRISA
            OVERPRISLISTA.START = 00.00
            OVERPRISLISTA.SLUT = 24.00.        
            GET CURRENT dq EXCLUSIVE-LOCK.
            ASSIGN
            DEFPRISLISTA.STARTDATUM = TODAY. 
         END.
      END.
      ELSE DO:
         FIND FIRST PRISLISTFAKT WHERE PRISLISTFAKT.PRISID = DEFPRISLISTA.PRISID AND
         PRISLISTFAKT.BEFATTNING = BEFATTNING.BEFATTNING NO-LOCK NO-ERROR.       
         IF NOT AVAILABLE PRISLISTFAKT THEN DO TRANSACTION:         
            CREATE PRISLISTFAKT.
            ASSIGN 
            PRISLISTFAKT.PRISID = DEFPRISLISTA.PRISID 
            PRISLISTFAKT.BEFATTNING = BEFATTNINGSTAB.BEFATTNING
            PRISLISTFAKT.PRISA = BEFATTNINGSTAB.PRISA.         
            GET CURRENT dq EXCLUSIVE-LOCK.
            ASSIGN
            DEFPRISLISTA.STARTDATUM = TODAY.      
         END.
      END. 
      GET NEXT dq NO-LOCK. 
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

