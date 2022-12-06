&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-1


/* Temp-Table and Buffer definitions                                    */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/05/02 - 12:41 pm

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
&Scoped-define NEW 
{GLOBVAR2DEL1.I}

&Scoped-define SHARED SHARED
{KONSTRMTRL.I}
DEFINE NEW SHARED VARIABLE konvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE valgruppapph AS HANDLE NO-UNDO.

DEFINE NEW SHARED TEMP-TABLE grupp_temp
   FIELD KONSKOD AS INTEGER
   FIELD BENAMNING AS CHARACTER
   FIELD ORDNING AS INTEGER
   INDEX ORD ORDNING ASCENDING.

DEFINE NEW SHARED TEMP-TABLE kon_temp
   FIELD KONSKOD AS INTEGER
   FIELD KTYPKOD AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   FIELD ORDNING AS INTEGER
   INDEX ORD ORDNING ASCENDING.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_GRUPP

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES konstgrptemp konsttemp

/* Definitions for BROWSE BRW_GRUPP                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_GRUPP konstgrptemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_GRUPP 
&Scoped-define QUERY-STRING-BRW_GRUPP FOR EACH konstgrptemp NO-LOCK ~
    BY konstgrptemp.BENAMNING
&Scoped-define OPEN-QUERY-BRW_GRUPP OPEN QUERY BRW_GRUPP FOR EACH konstgrptemp NO-LOCK ~
    BY konstgrptemp.BENAMNING.
&Scoped-define TABLES-IN-QUERY-BRW_GRUPP konstgrptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_GRUPP konstgrptemp


/* Definitions for BROWSE BRW_KON                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_KON konsttemp.KTYPKOD ~
konsttemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KON 
&Scoped-define QUERY-STRING-BRW_KON FOR EACH konsttemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KON OPEN QUERY BRW_KON FOR EACH konsttemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KON konsttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KON konsttemp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_GRUPP TOG_KON BRW_KON BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS TOG_KON 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE TOG_KON AS LOGICAL INITIAL no 
     LABEL "Konstruktion" 
     VIEW-AS TOGGLE-BOX
     SIZE 16.13 BY .88 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_GRUPP FOR 
      konstgrptemp SCROLLING.

DEFINE QUERY BRW_KON FOR 
      konsttemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_GRUPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_GRUPP WINDOW-1 _STRUCTURED
  QUERY BRW_GRUPP NO-LOCK DISPLAY
      konstgrptemp.BENAMNING FORMAT "X(256)":U WIDTH 40
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 45 BY 11.29
         TITLE "Välj konstruktion".

DEFINE BROWSE BRW_KON
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KON WINDOW-1 _STRUCTURED
  QUERY BRW_KON NO-LOCK DISPLAY
      konsttemp.KTYPKOD COLUMN-LABEL "Kod" FORMAT "X(8)":U WIDTH 11.38
      konsttemp.BENAMNING FORMAT "X(256)":U WIDTH 23
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 45 BY 12.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_GRUPP AT ROW 2 COL 1.63
     TOG_KON AT ROW 13.63 COL 1.5
     BRW_KON AT ROW 15.04 COL 1.63
     BTN_OK AT ROW 27.96 COL 17.63
     BTN_AVB AT ROW 27.96 COL 32.63
     "Välj konstruktioner!" VIEW-AS TEXT
          SIZE 24.5 BY 1.25 AT ROW 13.5 COL 1.5 WIDGET-ID 2
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 46.63 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: konstgrptemp T "?" NO-UNDO temp-db konstgrptemp
      TABLE: konsttemp T "?" NO-UNDO temp-db konsttemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Konstruktioner"
         HEIGHT             = 28.42
         WIDTH              = 46.75
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 46.75
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 46.75
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
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_GRUPP TEXT-1 FRAME-A */
/* BROWSE-TAB BRW_KON TOG_KON FRAME-A */
ASSIGN 
       BRW_GRUPP:MAX-DATA-GUESS IN FRAME FRAME-A         = 1000
       BRW_GRUPP:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

ASSIGN 
       BRW_KON:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_GRUPP
/* Query rebuild information for BROWSE BRW_GRUPP
     _TblList          = "Temp-Tables.konstgrptemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.konstgrptemp.BENAMNING|yes"
     _FldNameList[1]   > Temp-Tables.konstgrptemp.BENAMNING
"konstgrptemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_GRUPP */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KON
/* Query rebuild information for BROWSE BRW_KON
     _TblList          = "Temp-Tables.konsttemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.konsttemp.KTYPKOD
"konsttemp.KTYPKOD" "Kod" ? "character" ? ? ? ? ? ? no ? no no "11.38" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.konsttemp.BENAMNING
"konsttemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "23" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KON */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_GRUPP
&Scoped-define SELF-NAME BRW_GRUPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_GRUPP WINDOW-1
ON VALUE-CHANGED OF BRW_GRUPP IN FRAME FRAME-A /* Välj konstruktion */
DO:
   antal_valda = BRW_GRUPP:NUM-SELECTED-ROWS.
   IF antal_valda = 1 THEN BRW_GRUPP:SELECT-FOCUSED-ROW() NO-ERROR.
   RUN setcolsortvar_UI IN brwproc[2] (INPUT " WHERE konsttemp.KONSKOD = " + STRING(konstgrptemp.KONSKOD)).
   RUN openbdynspec_UI IN brwproc[2].
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avbryt */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK WINDOW-1
ON CHOOSE OF BTN_OK IN FRAME FRAME-A /* Ok */
DO:
   
   {muswait.i}
   EMPTY TEMP-TABLE grupp_temp NO-ERROR. 
   EMPTY TEMP-TABLE kon_temp NO-ERROR.    
   ASSIGN
   konvar = TRUE
   antal_valda = BRW_KON:NUM-SELECTED-ROWS.
   IF antal_valda < 1 THEN DO:
      MESSAGE "Ni har inte valt någon konstruktion."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
      RETURN NO-APPLY.
   END.
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = BRW_KON:FETCH-SELECTED-ROW(antal_raknare).                      
      IF AVAILABLE konsttemp THEN DO:
         CREATE kon_temp.
         ASSIGN
         kon_temp.KONSKOD = konsttemp.KONSKOD
         kon_temp.KTYPKOD = konsttemp.KTYPKOD
         kon_temp.BENAMNING = konsttemp.BENAMNING
         kon_temp.ORDNING = konsttemp.ORDNING.
      END.
      antal_raknare = antal_raknare + 1.   
   END.
                   
   status-ok = BRW_GRUPP:FETCH-SELECTED-ROW(1).
   IF AVAILABLE konstgrptemp THEN DO:
      CREATE grupp_temp.
      ASSIGN
      grupp_temp.KONSKOD = konstgrptemp.KONSKOD
      grupp_temp.BENAMNING = konstgrptemp.BENAMNING.
   END.
   RUN vkon_UI.
      


{musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_KON
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-1 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(valgruppapph) THEN DELETE PROCEDURE valgruppapph.
   RUN disable_UI.
END.
   

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
   {WIN_M_START.I}
   {muswait.i}
   {ALLSTARTDYN.I}
   TOG_KON = FALSE.
   RUN enable_UI.   
   {FRMSIZE.I}   
      /*
   BRW_KON:HIDDEN IN FRAME {&FRAME-NAME} = TRUE. 
   */
   TOG_KON = TRUE.
   RUN openbdynspec_UI IN brwproc[1].
   TOG_KON:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.   
   BRW_KON:HIDDEN IN FRAME {&FRAME-NAME} = FALSE. 
   APPLY "VALUE-CHANGED" TO BRW_GRUPP.
   {musarrow.i}   
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose:      
  Parameters:  
  Notes:       
-------------------------------------------------------------*/ 

   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_GRUPP:HANDLE IN FRAME {&FRAME-NAME}).    
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_KON:HANDLE IN FRAME {&FRAME-NAME}).    
   RUN setcolindex_UI IN brwproc[1] (INPUT "ORDNING").
   IF Guru.Konstanter:appcon THEN DO:
      RUN VALGRUPPAPP.P PERSISTENT SET valgruppapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN VALGRUPPAPP.P PERSISTENT SET valgruppapph.
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
  DISPLAY TOG_KON 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE BRW_GRUPP TOG_KON BRW_KON BTN_OK BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE vkon_UI WINDOW-1 
PROCEDURE vkon_UI :
/* -----------------------------------------------------------
  Purpose:      
  Parameters:  
  Notes:       
-------------------------------------------------------------*/ 

   {AVBGOM.I}
   RUN MTXTILLYKONST.W.
   {AVBFRAM.I}

   {&WINDOW-NAME}:HIDDEN = FALSE. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

