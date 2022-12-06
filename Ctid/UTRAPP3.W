&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-2



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-2 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/09/15 -  2:57 pm

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
{VPERSONALTEMP.I}
DEFINE INPUT PARAMETER TABLE FOR valperstemp.
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{REGVAR.I}
&Scoped-define SHARED SHARED

DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.         
DEFINE VARIABLE str AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
{TIDUTTTNEW.I}
DEFINE SHARED TEMP-TABLE tidut1
   FIELD UT AS CHARACTER FORMAT "X(92)".
DEFINE SHARED TEMP-TABLE tidut2
   FIELD UT AS CHARACTER FORMAT "X(92)".
DEFINE SHARED TEMP-TABLE tidut3
   FIELD UT AS CHARACTER FORMAT "X(92)".
DEFINE {&NEW} SHARED VARIABLE RAD_SORTPERS AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-TIDS
&Scoped-define BROWSE-NAME BRW_UT1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tidut1 tidut2 tidut3

/* Definitions for BROWSE BRW_UT1                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_UT1 tidut1.ut 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UT1 
&Scoped-define QUERY-STRING-BRW_UT1 FOR EACH tidut1 NO-LOCK
&Scoped-define OPEN-QUERY-BRW_UT1 OPEN QUERY BRW_UT1 FOR EACH tidut1 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_UT1 tidut1
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UT1 tidut1


/* Definitions for BROWSE BRW_UT2                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_UT2 tidut2.ut 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UT2 
&Scoped-define QUERY-STRING-BRW_UT2 FOR EACH tidut2 NO-LOCK
&Scoped-define OPEN-QUERY-BRW_UT2 OPEN QUERY BRW_UT2 FOR EACH tidut2 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_UT2 tidut2
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UT2 tidut2


/* Definitions for BROWSE BRW_UT3                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_UT3 tidut3.ut 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UT3 
&Scoped-define QUERY-STRING-BRW_UT3 FOR EACH tidut3 NO-LOCK
&Scoped-define OPEN-QUERY-BRW_UT3 OPEN QUERY BRW_UT3 FOR EACH tidut3 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_UT3 tidut3
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UT3 tidut3


/* Definitions for FRAME FRAME-TIDS                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-TIDS ~
    ~{&OPEN-QUERY-BRW_UT1}~
    ~{&OPEN-QUERY-BRW_UT2}~
    ~{&OPEN-QUERY-BRW_UT3}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RAD_VAL BRW_UT1 BRW_UT2 BRW_UT3 BTN_SKRIV ~
BTN_AVS 
&Scoped-Define DISPLAYED-OBJECTS RAD_VAL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SKRIV 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE VARIABLE RAD_VAL AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Personuppgifter", 1,
"Adresser", 2,
"Avtal", 3
     SIZE 44.5 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_UT1 FOR 
      tidut1 SCROLLING.

DEFINE QUERY BRW_UT2 FOR 
      tidut2 SCROLLING.

DEFINE QUERY BRW_UT3 FOR 
      tidut3 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_UT1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UT1 WINDOW-2 _STRUCTURED
  QUERY BRW_UT1 NO-LOCK DISPLAY
      tidut1.ut FORMAT "X(92)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 95.5 BY 24.5.

DEFINE BROWSE BRW_UT2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UT2 WINDOW-2 _STRUCTURED
  QUERY BRW_UT2 NO-LOCK DISPLAY
      tidut2.ut FORMAT "X(92)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 95.5 BY 24.5.

DEFINE BROWSE BRW_UT3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UT3 WINDOW-2 _STRUCTURED
  QUERY BRW_UT3 NO-LOCK DISPLAY
      tidut3.ut FORMAT "X(92)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-COLUMN-SCROLLING SIZE 95.5 BY 24.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-TIDS
     RAD_VAL AT ROW 1.5 COL 1.5 NO-LABEL
     BRW_UT1 AT ROW 2.5 COL 1.5
     BRW_UT2 AT ROW 2.5 COL 1.5
     BRW_UT3 AT ROW 2.5 COL 1.5
     BTN_SKRIV AT ROW 27.5 COL 67.38
     BTN_AVS AT ROW 27.5 COL 83
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 97 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Temp-Tables and Buffers:
      TABLE: tidut1 T "?" NO-UNDO temp-db tidut1
      TABLE: tidut2 T "?" NO-UNDO temp-db tidut2
      TABLE: tidut3 T "?" NO-UNDO temp-db tidut3
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Personalregister"
         HEIGHT             = 28.42
         WIDTH              = 97
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 97
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 97
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
/* SETTINGS FOR WINDOW WINDOW-2
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-TIDS
                                                                        */
/* BROWSE-TAB BRW_UT1 RAD_VAL FRAME-TIDS */
/* BROWSE-TAB BRW_UT2 BRW_UT1 FRAME-TIDS */
/* BROWSE-TAB BRW_UT3 BRW_UT2 FRAME-TIDS */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UT1
/* Query rebuild information for BROWSE BRW_UT1
     _TblList          = "Temp-Tables.tidut1"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.tidut1.ut
"tidut1.ut" ? "X(92)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_UT1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UT2
/* Query rebuild information for BROWSE BRW_UT2
     _TblList          = "Temp-Tables.tidut2"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.tidut2.ut
"tidut2.ut" ? "X(92)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_UT2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UT3
/* Query rebuild information for BROWSE BRW_UT3
     _TblList          = "Temp-Tables.tidut3"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.tidut3.ut
"tidut3.ut" ? "X(92)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_UT3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-TIDS
/* Query rebuild information for FRAME FRAME-TIDS
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-TIDS */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS WINDOW-2
ON CHOOSE OF BTN_AVS IN FRAME FRAME-TIDS /* Avsluta */
DO:
   {BORTBRWPROC.I}
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKRIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON CHOOSE OF BTN_SKRIV IN FRAME FRAME-TIDS /* Skriv ut */
DO:
   RUN bufcop_UI.
   RUN SKRIVVAL.W (INPUT FALSE).       
   IF musz = TRUE THEN musz = FALSE. 
   ELSE DO:    
      RUN ut_UI.
      skrivut = FALSE.      
   END.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKRIV WINDOW-2
ON MOUSE-MENU-CLICK OF BTN_SKRIV IN FRAME FRAME-TIDS /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_VAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_VAL WINDOW-2
ON VALUE-CHANGED OF RAD_VAL IN FRAME FRAME-TIDS
DO:
   ASSIGN RAD_VAL = INPUT RAD_VAL.
   ASSIGN
   BRW_UT1:HIDDEN = TRUE
   BRW_UT2:HIDDEN = TRUE
   BRW_UT3:HIDDEN = TRUE.
   IF RAD_VAL = 1 THEN DO:
      BRW_UT1:HIDDEN = FALSE.
   END.
   IF RAD_VAL = 2 THEN DO:
      BRW_UT2:HIDDEN = FALSE.
   END.
   IF RAD_VAL = 3 THEN DO:
      BRW_UT3:HIDDEN = FALSE.        
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_UT1
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-2 


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
   {WIN_M_START.I}
   {muswait.i}
   {ALLSTARTDYN.I}       
   FOR EACH tidut1:
      DELETE tidut1.
   END.
   FOR EACH tidut2:
      DELETE tidut2.
   END.
   FOR EACH tidut3:
      DELETE tidut3.
   END.
   ENABLE BRW_UT1 BRW_UT2 BRW_UT3 WITH FRAME {&FRAME-NAME}.              
   RAD_VAL = 1.
   RUN enable_UI.
   {FRMSIZE.I}
   ASSIGN
   Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = BTN_AVS:HANDLE.           
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = BTN_SKRIV:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   APPLY "VALUE-CHANGED" TO RAD_VAL.
   RUN nytt_UI.
   skrivut = FALSE.   
   {musarrow.i}    
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-2 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_UT1:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_UT2:HANDLE IN FRAME {&FRAME-NAME}).
   RUN DYNBRW.P PERSISTENT SET brwproc[3]
      (INPUT BRW_UT3:HANDLE IN FRAME {&FRAME-NAME}).
  
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bufcop_UI WINDOW-2 
PROCEDURE bufcop_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   FOR EACH tidut:
      DELETE tidut.
   END.
   IF RAD_VAL = 1 THEN DO:
      FOR EACH tidut1:
         CREATE tidut.
         BUFFER-COPY tidut1 TO tidut.
      END.
   END.
   IF RAD_VAL = 2 THEN DO:
      FOR EACH tidut2:
         CREATE tidut.
         BUFFER-COPY tidut2 TO tidut.
      END.
   END.
   IF RAD_VAL = 3 THEN DO:
      FOR EACH tidut3:
         CREATE tidut.
         BUFFER-COPY tidut3 TO tidut.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-2  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
  THEN DELETE WIDGET WINDOW-2.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-2  _DEFAULT-ENABLE
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
  DISPLAY RAD_VAL 
      WITH FRAME FRAME-TIDS IN WINDOW WINDOW-2.
  ENABLE RAD_VAL BRW_UT1 BRW_UT2 BRW_UT3 BTN_SKRIV BTN_AVS 
      WITH FRAME FRAME-TIDS IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-TIDS}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nytt_UI WINDOW-2 
PROCEDURE nytt_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN UTRAP3.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT RAD_SORTPERS,INPUT RAD_VAL,INPUT TABLE valperstemp,OUTPUT TABLE tidut1,OUTPUT TABLE tidut2,OUTPUT TABLE tidut3).
   END.
   ELSE DO:
      RUN UTRAP3.P  
      (INPUT RAD_SORTPERS,INPUT RAD_VAL,INPUT TABLE valperstemp,OUTPUT TABLE tidut1,OUTPUT TABLE tidut2,OUTPUT TABLE tidut3).
   END. 
   OPEN QUERY BRW_UT1 FOR EACH tidut1 NO-LOCK.
   OPEN QUERY BRW_UT2 FOR EACH tidut2 NO-LOCK.
   OPEN QUERY BRW_UT3 FOR EACH tidut3 NO-LOCK.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut_UI WINDOW-2 
PROCEDURE ut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  /*UT*/ 
   FIND LAST tidut NO-LOCK NO-ERROR.     
   RUN EKLOGS.P. 
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

