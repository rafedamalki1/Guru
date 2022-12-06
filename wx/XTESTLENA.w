&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
/*          This .W file was created with the Progress AppBuilder.      */
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
{HUSEGENTEMP.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_HUS

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES husegentemp

/* Definitions for BROWSE BRW_HUS                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_HUS husegentemp.VIHUSEGENSKAP ~
husegentemp.OBLIGA 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_HUS husegentemp.VIHUSEGENSKAP ~
husegentemp.OBLIGA 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_HUS husegentemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_HUS husegentemp
&Scoped-define QUERY-STRING-BRW_HUS FOR EACH husegentemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_HUS OPEN QUERY BRW_HUS FOR EACH husegentemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_HUS husegentemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_HUS husegentemp


/* Definitions for BROWSE BRW_HUS2                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_HUS2 husegentemp.VIHUSEGENSKAP ~
husegentemp.OBLIGA 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_HUS2 husegentemp.VIHUSEGENSKAP ~
husegentemp.OBLIGA 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_HUS2 husegentemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_HUS2 husegentemp
&Scoped-define QUERY-STRING-BRW_HUS2 FOR EACH husegentemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_HUS2 OPEN QUERY BRW_HUS2 FOR EACH husegentemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_HUS2 husegentemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_HUS2 husegentemp


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BRW_HUS2}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BTN_AVB BRW_HUS BRW_HUS2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-GO 
     LABEL "Avbryt":L 
     SIZE 10.5 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_HUS FOR 
      husegentemp SCROLLING.

DEFINE QUERY BRW_HUS2 FOR 
      husegentemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_HUS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_HUS C-Win _STRUCTURED
  QUERY BRW_HUS NO-LOCK DISPLAY
      husegentemp.VIHUSEGENSKAP COLUMN-LABEL "Husegenskap" FORMAT "X(256)":U
            WIDTH 20
      husegentemp.OBLIGA COLUMN-LABEL "Obligatoriskt" FORMAT "Ja/Nej":U
  ENABLE
      husegentemp.VIHUSEGENSKAP
      husegentemp.OBLIGA
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 48.5 BY 3.55
         TITLE "Husegenskaper".

DEFINE BROWSE BRW_HUS2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_HUS2 C-Win _STRUCTURED
  QUERY BRW_HUS2 NO-LOCK DISPLAY
      husegentemp.VIHUSEGENSKAP FORMAT "X(256)":U WIDTH 20
      husegentemp.OBLIGA FORMAT "Ja/Nej":U
  ENABLE
      husegentemp.VIHUSEGENSKAP
      husegentemp.OBLIGA
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 59.5 BY 9.55 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BTN_AVB AT ROW 1.55 COL 52.5
     BRW_HUS AT ROW 4 COL 4
     BRW_HUS2 AT ROW 10.82 COL 5.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 78.88 BY 23.5.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: husegentemp T "?" NO-UNDO temp-db husegentemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 25.64
         WIDTH              = 79.63
         MAX-HEIGHT         = 25.64
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 25.64
         VIRTUAL-WIDTH      = 80
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
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BRW_HUS BTN_AVB DEFAULT-FRAME */
/* BROWSE-TAB BRW_HUS2 BRW_HUS DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_HUS
/* Query rebuild information for BROWSE BRW_HUS
     _TblList          = "Temp-Tables.husegentemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.husegentemp.VIHUSEGENSKAP
"husegentemp.VIHUSEGENSKAP" "Husegenskap" "X(256)" "character" ? ? ? ? ? ? yes ? no no "20" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.husegentemp.OBLIGA
"husegentemp.OBLIGA" "Obligatoriskt" ? "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_HUS */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_HUS2
/* Query rebuild information for BROWSE BRW_HUS2
     _TblList          = "Temp-Tables.husegentemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.husegentemp.VIHUSEGENSKAP
"VIHUSEGENSKAP" ? "X(256)" "character" ? ? ? ? ? ? yes ? no no "20" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.husegentemp.OBLIGA
"OBLIGA" ? ? "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_HUS2 */
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


&Scoped-define BROWSE-NAME BRW_HUS
&Scoped-define SELF-NAME BRW_HUS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_HUS C-Win
ON ROW-LEAVE OF BRW_HUS IN FRAME DEFAULT-FRAME /* Husegenskaper */
DO:
  /*IF AVAILABLE husegentemp THEN DO:
     RUN visa_UI.    
     ASSIGN     
     husegentemp.VIHUSEGENSKAP = INPUT BROWSE BRW_HUS husegentemp.VIHUSEGENSKAP
     husegentemp.OBLIGA = INPUT BROWSE BRW_HUS husegentemp.OBLIGA.
     RUN visa_UI.         
  END.               */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avbryt */
DO:   
   
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON GO OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
   RETURN.
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
    CREATE husegentemp.
    ASSIGN
    husegentemp.vihusegenskap = "a"
    husegentemp.obliga = TRUE.
    CREATE husegentemp.
    ASSIGN
    husegentemp.vihusegenskap = "a"
    husegentemp.obliga = TRUE.
    CREATE husegentemp.
    ASSIGN
    husegentemp.vihusegenskap = "a"
    husegentemp.obliga = TRUE.
    CREATE husegentemp.
    ASSIGN
    husegentemp.vihusegenskap = "a"
    husegentemp.obliga = TRUE.
    CREATE husegentemp.
    ASSIGN
    husegentemp.vihusegenskap = "a"
    husegentemp.obliga = TRUE.
    CREATE husegentemp.
    ASSIGN
    husegentemp.vihusegenskap = "a"
    husegentemp.obliga = TRUE.
    CREATE husegentemp.
    ASSIGN
    husegentemp.vihusegenskap = "a"
    husegentemp.obliga = TRUE.
    CREATE husegentemp.
    ASSIGN
    husegentemp.vihusegenskap = "a"
    husegentemp.obliga = TRUE.
    CREATE husegentemp.
    ASSIGN
    husegentemp.vihusegenskap = "a"
    husegentemp.obliga = TRUE.
    CREATE husegentemp.
    ASSIGN
    husegentemp.vihusegenskap = "a"
    husegentemp.obliga = TRUE.
    CREATE husegentemp.
    ASSIGN
    husegentemp.vihusegenskap = "a"
    husegentemp.obliga = TRUE.
    CREATE husegentemp.
    ASSIGN
    husegentemp.vihusegenskap = "a"
    husegentemp.obliga = TRUE.
    CREATE husegentemp.
    ASSIGN
    husegentemp.vihusegenskap = "a"
    husegentemp.obliga = TRUE.
    CREATE husegentemp.
    ASSIGN
    husegentemp.vihusegenskap = "a"
    husegentemp.obliga = TRUE.
    CREATE husegentemp.
    ASSIGN
    husegentemp.vihusegenskap = "h"
    husegentemp.obliga = TRUE.
  RUN enable_UI.
  OPEN QUERY brw_hus FOR EACH  husegentemp.    
  OPEN QUERY brw_hus2 FOR EACH  husegentemp.    
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
  ENABLE BTN_AVB BRW_HUS BRW_HUS2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

