&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-4


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-4 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 12/18/97 -  8:54 am

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

&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{APPARAT.I}
DEFINE NEW SHARED VARIABLE andra AS LOGICAL NO-UNDO.       
DEFINE VARIABLE approwid AS ROWID NO-UNDO.  
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.     
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO. 
DEFINE VARIABLE svar AS LOGICAL NO-UNDO.
DEFINE VARIABLE apparadmapph AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-D
&Scoped-define BROWSE-NAME BRW_YT

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES apparattemp

/* Definitions for BROWSE BRW_YT                                        */
&Scoped-define FIELDS-IN-QUERY-BRW_YT apparattemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_YT 
&Scoped-define QUERY-STRING-BRW_YT FOR EACH apparattemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_YT OPEN QUERY BRW_YT FOR EACH apparattemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_YT apparattemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_YT apparattemp


/* Definitions for FRAME FRAME-D                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-D ~
    ~{&OPEN-QUERY-BRW_YT}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_YT BTN_NY BTN_AND BTN_BORT BTN_AVB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-4 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AND 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_AVB AUTO-END-KEY 
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
DEFINE QUERY BRW_YT FOR 
      apparattemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_YT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_YT WINDOW-4 _STRUCTURED
  QUERY BRW_YT NO-LOCK DISPLAY
      apparattemp.BENAMNING FORMAT "X(60)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 63.13 BY 25.58
         TITLE "Apparater".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-D
     BRW_YT AT ROW 1.67 COL 4.88
     BTN_NY AT ROW 27.92 COL 15.13
     BTN_AND AT ROW 27.92 COL 30.13
     BTN_BORT AT ROW 27.92 COL 45.13
     BTN_AVB AT ROW 27.92 COL 69
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 82.75 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: apparattemp T "?" NO-UNDO temp-db apparattemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-4 ASSIGN
         HIDDEN             = YES
         TITLE              = "Administration av apparater"
         HEIGHT             = 28.42
         WIDTH              = 82.88
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 82.88
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 82.88
         RESIZE             = yes
         SCROLL-BARS        = yes
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
/* SETTINGS FOR WINDOW WINDOW-4
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-D
                                                                        */
/* BROWSE-TAB BRW_YT 1 FRAME-D */
ASSIGN 
       BRW_YT:HIDDEN  IN FRAME FRAME-D                = TRUE
       BRW_YT:MAX-DATA-GUESS IN FRAME FRAME-D         = 1000
       BRW_YT:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-D = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-4)
THEN WINDOW-4:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_YT
/* Query rebuild information for BROWSE BRW_YT
     _TblList          = "Temp-Tables.apparattemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.apparattemp.BENAMNING
"apparattemp.BENAMNING" ? "X(60)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_YT */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_YT
&Scoped-define SELF-NAME BRW_YT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_YT WINDOW-4
ON VALUE-CHANGED OF BRW_YT IN FRAME FRAME-D /* Apparater */
DO:
   status-ok = BRW_YT:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AND WINDOW-4
ON CHOOSE OF BTN_AND IN FRAME FRAME-D /* Ändra */
DO:
   {muswait.i}                        
   FIND FIRST apparattemp NO-LOCK NO-ERROR.
   IF AVAILABLE apparattemp THEN DO:
      RUN fetchselrowid_UI IN brwproc[1].
      ASSIGN
      approwid = apparattemp.APROW. 
      andra = TRUE.
      
      RUN NYAPPU.W (INPUT andra,INPUT-OUTPUT approwid).
      
      IF musz = FALSE THEN DO:
         RUN openbdyn_UI IN brwproc[1] (INPUT "").            
         FIND FIRST apparattemp WHERE apparattemp.APROW = approwid NO-LOCK NO-ERROR.
         IF AVAILABLE apparattemp THEN DO:
            RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(apparattemp)).
            RUN lastselectdyn_UI IN brwproc[1].
         END.
      END.
      
      andra = FALSE.
   END.
   ELSE DO:
      MESSAGE "Det finns ingen apparat att ändra."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.       
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-4
ON CHOOSE OF BTN_BORT IN FRAME FRAME-D /* Ta bort */
DO: 
   FIND FIRST apparattemp NO-LOCK NO-ERROR.
   IF AVAILABLE apparattemp THEN DO:
      RUN fetchselrowid_UI IN brwproc[1].
      approwid = apparattemp.APROW.
      MESSAGE "Vill du ta bort " + apparattemp.BENAMNING + "?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Ta bort?" UPDATE svar.         
      IF svar THEN DO:  
         RUN bort_UI IN apparadmapph (INPUT approwid).
         FIND FIRST apparattemp WHERE apparattemp.APROW = approwid NO-LOCK NO-ERROR.
         IF AVAILABLE apparattemp THEN DO:
            DELETE apparattemp.
            RUN selnextprevrow_UI IN brwproc[1].
            RUN openbdyn_UI IN brwproc[1] (INPUT ""). 
            RUN lastselectdyn_UI IN brwproc[1].
         END.         
      END.   
   END.
   ELSE DO:
      MESSAGE "Det finns ingen apparat att ta bort."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WINDOW-4
ON CHOOSE OF BTN_NY IN FRAME FRAME-D /* Ny */
DO:
   {muswait.i}       
   andra = FALSE.
   
   RUN NYAPPU.W (INPUT andra,INPUT-OUTPUT approwid).
   
   IF musz = FALSE THEN DO:
      RUN openbdyn_UI IN brwproc[1] (INPUT "").            
      FIND FIRST apparattemp WHERE apparattemp.APROW = approwid NO-LOCK NO-ERROR.
      IF AVAILABLE apparattemp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(apparattemp)).
         RUN lastselectdyn_UI IN brwproc[1].
      END.
   END.
   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-4 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(apparadmapph) THEN DELETE PROCEDURE apparadmapph.
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
   {ALLSTARTDYN.I}
   {muswait.i}
   RUN laddaapparat_UI IN apparadmapph (OUTPUT TABLE apparattemp).
   RUN enable_UI.   
   {FRMSIZE.I}             
   ASSIGN  
   BRW_YT:HIDDEN IN FRAME {&FRAME-NAME} = FALSE. 
   RUN openbdyn_UI IN brwproc[1] (INPUT "").  
   {musarrow.i}
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-4 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose:      
  Parameters:  
  Notes:       
-------------------------------------------------------------*/ 
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_YT:HANDLE IN FRAME {&FRAME-NAME}). 
   RUN brwsetupstop_UI IN brwproc[1] (INPUT 1).
   IF Guru.Konstanter:appcon THEN DO:
      RUN APPARATAPP.P PERSISTENT SET apparadmapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN APPARATAPP.P PERSISTENT SET apparadmapph.
   END.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-4  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-4)
  THEN DELETE WIDGET WINDOW-4.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-4  _DEFAULT-ENABLE
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
  ENABLE BRW_YT BTN_NY BTN_AND BTN_BORT BTN_AVB 
      WITH FRAME FRAME-D IN WINDOW WINDOW-4.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-D}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

