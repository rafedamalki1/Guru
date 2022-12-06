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
RUN wc-start.w.
RETURN.
CREATE WIDGET-POOL.


/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{NAMNDB.I}
{ALLDEF2.I}
{APPCONDEF.I}
{VALDBDEF.I}
   /*
OUTPUT TO c:\wc.txt APPEND.
PUT STRING(TIME,"hh:mm:ss") AT 1 
     STRING(TODAY)          AT 12
     SKIP.
OUTPUT CLOSE.
QUIT.
*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_VDB

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES valdbtemp

/* Definitions for BROWSE BRW_VDB                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_VDB valdbtemp.FORETAG valdbtemp.VALDB 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_VDB 
&Scoped-define QUERY-STRING-BRW_VDB FOR EACH valdbtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_VDB OPEN QUERY BRW_VDB FOR EACH valdbtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_VDB valdbtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_VDB valdbtemp


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BRW_VDB}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_VDB FILL-IN-APPSERVER BTN_START BTN_TA ~
BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-APPSERVER 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_START AUTO-GO 
     LABEL "Starta Guru" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_TA AUTO-GO 
     LABEL "Ta bort användare" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN-APPSERVER AS LOGICAL FORMAT "Ja/Nej":U INITIAL YES 
     LABEL "APPSERVER" 
     VIEW-AS FILL-IN 
     SIZE 4.5 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_VDB FOR 
      valdbtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_VDB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_VDB C-Win _STRUCTURED
  QUERY BRW_VDB DISPLAY
      valdbtemp.FORETAG FORMAT "X(5)":U
      valdbtemp.VALDB COLUMN-LABEL "Databas" FORMAT "X(40)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 33.5 BY 26.54
         FONT 4
         TITLE "Databaser" TOOLTIP "Välj databas".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BRW_VDB AT ROW 2.71 COL 1.5
     FILL-IN-APPSERVER AT ROW 6.5 COL 42.88 COLON-ALIGNED
     BTN_START AT ROW 8 COL 36.38
     BTN_TA AT ROW 9.08 COL 36.38
     BTN_AVB AT ROW 28.17 COL 36.38
     "Välj databas för start av Guru !" VIEW-AS TEXT
          SIZE 35.25 BY 1 AT ROW 1.5 COL 1.5
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 50.5 BY 28.42
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db valdbtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Start av Guru"
         HEIGHT             = 28.42
         WIDTH              = 50.25
         MAX-HEIGHT         = 29.83
         MAX-WIDTH          = 87.25
         VIRTUAL-HEIGHT     = 29.83
         VIRTUAL-WIDTH      = 87.25
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
/* BROWSE-TAB BRW_VDB TEXT-1 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_VDB
/* Query rebuild information for BROWSE BRW_VDB
     _TblList          = "Temp-Tables.valdbtemp"
     _FldNameList[1]   = Temp-Tables.valdbtemp.FORETAG
     _FldNameList[2]   > Temp-Tables.valdbtemp.VALDB
"valdbtemp.VALDB" "Databas" "X(40)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_VDB */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Start av Guru */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Start av Guru */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_VDB
&Scoped-define SELF-NAME BRW_VDB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VDB C-Win
ON MOUSE-SELECT-DBLCLICK OF BRW_VDB IN FRAME DEFAULT-FRAME /* Databaser */
DO:
   APPLY "CHOOSE" TO BTN_START.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VDB C-Win
ON ROW-DISPLAY OF BRW_VDB IN FRAME DEFAULT-FRAME /* Databaser */
DO:
   
   IF valdbtemp.VALDB = "Vattenfall Service AB" OR valdbtemp.VALDB = "Graninge Nät AB" OR 
      valdbtemp.VALDB = "Sundsvall Energi Elnät AB"THEN DO:
      ASSIGN
      valdbtemp.FORETAG:FGCOLOR IN BROWSE {&BROWSE-NAME} = 12. 
      valdbtemp.VALDB:FGCOLOR IN BROWSE {&BROWSE-NAME} = 12. 
   END.
      
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_VDB C-Win
ON VALUE-CHANGED OF BRW_VDB IN FRAME DEFAULT-FRAME /* Databaser */
DO:
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:       
  /* IF Guru.Konstanter:globforetag = "SOLE" THEN QUIT.*/
    status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   SESSION:PRINTER-CONTROL-HANDLE = 0.
   IF isweb = TRUE THEN RETURN.
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   IF NOT AVAILABLE valdbtemp THEN musz = musz.
   ELSE DO:
      IF namndb() NE valdbtemp.DBNAMN THEN DO TRANSACTION:
         IF namndb() NE ? THEN DISCONNECT VALUE(namndb()) NO-ERROR.         
      END.
      IF NOT CONNECTED(valdbtemp.DBNAMN) THEN DO :
         RUN con_UI.            
      END.
      varmess = "".
      IF ERROR-STATUS:NUM-MESSAGES > 0  THEN DO:
         DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
            IF ERROR-STATUS:GET-NUMBER(i) = 6126 THEN DO:
               ASSIGN
               varerror = i
               varmess = "6126".         
            END.
         END.
         IF ERROR-STATUS:GET-NUMBER(ERROR-STATUS:NUM-MESSAGES) = 4069 THEN musz = musz.
         ELSE IF ERROR-STATUS:GET-NUMBER(ERROR-STATUS:NUM-MESSAGES) = 565 THEN musz = musz.
         ELSE RUN medd_UI.
      END.
      IF CONNECTED(valdbtemp.DBNAMN) THEN DO:
         {VERALIAS.I}
         
         RUN FORVER.P (INPUT valdbtemp.GFORETAG,INPUT valdbtemp.APPCON).         
         
         musz = FALSE.
      END.
      ELSE musz = TRUE.      
   END.
   /*
   RUN val_UI.
   */
 /*  DEFAULT-WINDOW:HIDDEN = FALSE.*/
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON ENDKEY OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
  SESSION:PRINTER-CONTROL-HANDLE = 0.
  RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_START
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_START C-Win
ON CHOOSE OF BTN_START IN FRAME DEFAULT-FRAME /* Starta Guru */
DO:  
   FILL-IN-APPSERVER = INPUT FILL-IN-APPSERVER.
   
   IF valdbtemp.GFORETAG = "INGEN" THEN DO:
      MESSAGE valdbtemp.VALDB
      VIEW-AS ALERT-BOX.
      RUN NYASERVERPROG.p (INPUT valdbtemp.FORETAG).
   END.   
   
   ELSE IF FILL-IN-APPSERVER = TRUE THEN DO:
      RUN val_UI.
      IF musz = FALSE THEN DO:
          MESSAGE valdbtemp.APPCON
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
             
          IF valdbtemp.GFORETAG = "dELPA" OR valdbtemp.GFORETAG = "classELPA" THEN demokvar = TRUE.
          ELSE demokvar = FALSE.
         {valstart.i}      
      END.
   END.
   ELSE DO:
      RUN ejappcon_UI.
   END.
   musz = FALSE.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_TA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_TA C-Win
ON CHOOSE OF BTN_TA IN FRAME DEFAULT-FRAME /* Ta bort användare */
DO:
   
   DISABLE BRW_VDB BTN_START WITH FRAME {&FRAME-NAME}.
   RUN KUNDNER.W.
   ENABLE BRW_VDB BTN_START WITH FRAME {&FRAME-NAME}.   
   
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
   /*{WIN_M_START.I}*/   
   /*CMB_DB:SCREEN-VALUE = "Energiadministration". */
   {VALDBALL.I}  
   {VALDBMAIN.I}
   {SLUTWIN.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE con_UI C-Win 
PROCEDURE con_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   kommando = valdbtemp.DBPLATS + valdbtemp.DBNAMN.
   kommando = kommando + " -P 'KAGGEN' -U 'ELPAO'".
   CONNECT VALUE(kommando) NO-ERROR. 
   IF NOT CONNECTED(valdbtemp.DBNAMN) THEN DO:   
      kommando = valdbtemp.DBCON.
      kommando = REPLACE(kommando,"www.guruonweb.se","webguru") NO-ERROR.
      kommando = REPLACE(kommando,"www2.guruonweb.se","webguru") NO-ERROR.
      kommando = kommando + " -P 'KAGGEN' -U 'ELPAO'".  
      CONNECT VALUE(kommando) NO-ERROR. 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ejappcon_UI C-Win 
PROCEDURE ejappcon_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   DEFINE VARIABLE nyaprog AS LOGICAL NO-UNDO.
   BTN_AVB:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   {&WINDOW-NAME}:HIDDEN = TRUE.
   DEFAULT-WINDOW:HIDDEN = TRUE.
   {&WINDOW-NAME}:ALWAYS-ON-TOP = FALSE.       
   {&WINDOW-NAME}:HIDDEN = TRUE.      
   DISABLE BRW_VDB BTN_START WITH FRAME {&FRAME-NAME}.      
   Guru.SharedVariable:singel = FALSE.     
   MESSAGE valdbtemp.DBPLATS valdbtemp.DBNAMN 
   VIEW-AS ALERT-BOX.
   {VALDBVAL2.I}
   IF musz = FALSE THEN DO:

      nyaprog = TRUE.
      REPEAT:
         RUN WSTART.W (INPUT valdbtemp.GFORETAG,INPUT valdbtemp.DBCACHE, INPUT-OUTPUT nyaprog).
         IF nyaprog = FALSE THEN LEAVE.
      END.
   END.
   ENABLE BRW_VDB BTN_START WITH FRAME {&FRAME-NAME}.
   BTN_AVB:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   {&WINDOW-NAME}:HIDDEN = FALSE.
   {&WINDOW-NAME}:HIDDEN = FALSE.
   {&WINDOW-NAME}:MOVE-TO-TOP ().
   {musarrow.i}
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
  DISPLAY FILL-IN-APPSERVER 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BRW_VDB FILL-IN-APPSERVER BTN_START BTN_TA BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE medd_UI C-Win 
PROCEDURE medd_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   MESSAGE 
   "Anslutningen till " valdbtemp.VALDB " misslyckades!" SKIP
   ERROR-STATUS:NUM-MESSAGES 
   " fel uppkom vid anslutningen." SKIP 
   "Vill du se dem ?" 
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
   UPDATE view-errs AS LOGICAL.       
   IF view-errs THEN DO i = 1 TO ERROR-STATUS:NUM-MESSAGES:
      MESSAGE ERROR-STATUS:GET-NUMBER(i)
      ERROR-STATUS:GET-MESSAGE(i)
      VIEW-AS ALERT-BOX.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE val_UI C-Win 
PROCEDURE val_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
  /* APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.*/
   
   
   {musarrow.i}       
   {VALDBVAL.I}
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

