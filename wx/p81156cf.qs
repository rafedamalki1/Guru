 _VERSION-NUMBER UIB_v9r12 GUI


 _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 

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
DEF VAR gaok AS CHAR.
DEFINE TEMP-TABLE tidtemp
   FIELD AONR AS char
   FIELD befattning AS char
   FIELD TIDLOG AS LOGICAL FORMAT "TID/TILL"
   INDEX aonr aonr.
gaok = "vvv".
/* _UIB-CODE-BLOCK-END */

 _UIB-PREPROCESSOR-BLOCK 


/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tidtemp

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 tidtemp.TIDLOG tidtemp.AONR ~
SUBSTRING (tidtemp.BEFATTNING,2,10) 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH tidtemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 tidtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 tidtemp


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */



DEFINE QUERY BROWSE-1 FOR 
      tidtemp SCROLLING.

 _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 C-Win _STRUCTURED

  QUERY BROWSE-1 NO-LOCK DISPLAY
      tidtemp.TIDLOG FORMAT "yes/no":U
      tidtemp.AONR FORMAT "X(6)":U
      SUBSTRING (tidtemp.BEFATTNING,2,10) COLUMN-LABEL gaok FORMAT "x(10)":U
            WIDTH 9.5
/* _UIB-CODE-BLOCK-END */

 _PROCEDURE-SETTINGS

/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db tidtemp
   END-TABLES.
 */
_END-PROCEDURE-SETTINGS
 _CREATE-WINDOW

IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 16
         WIDTH              = 36.88
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
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

 _RUN-TIME-ATTRIBUTES

/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BROWSE-1 1 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */

 _QUERY-BLOCK BROWSE BROWSE-1

/* Query rebuild information for BROWSE BROWSE-1
     _TblList          = "Temp-Tables.tidtemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = Temp-Tables.tidtemp.TIDLOG
     _FldNameList[2]   = Temp-Tables.tidtemp.AONR
     _FldNameList[3]   > "_<CALC>"
"SUBSTRING (tidtemp.BEFATTNING,2,10)" "test" "x(10)" ? ? ? ? ? ? ? no ? no no "9.5" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */

 _UIB-CODE-BLOCK _CONTROL C-Win C-Win

ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _CONTROL C-Win C-Win

ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _CONTROL BROWSE-1 C-Win

ON ROW-DISPLAY OF BROWSE-1 IN FRAME DEFAULT-FRAME
DO:
   tidtemp.tidlog:FORMAT IN BROWSE browse-1 = "ja/nej".
   
END.

/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 



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
   DEF VAR i AS INT.
   REPEAT:
      
      CREATE tidtemp.
      tidtemp.aonr = STRING(i).
      tidtemp.tidlog = TRUE.
      tidtemp.befattning = STRING(i + 10).
      i = i + 2.
      IF i > 15 THEN LEAVE.
   END.
   i = 1.
   REPEAT:
      
      CREATE tidtemp.
      tidtemp.aonr = STRING(i).
      tidtemp.tidlog = false.
      tidtemp.befattning = STRING(i + 10).
      i = i + 2.
      IF i > 15 THEN LEAVE.
   END.
 /*     SUBSTRING (tidtemp.BEFATTNING,2,10) COLUMN-LABEL "test" FORMAT "x(10)":U
   AONRTAB.AONR:LABEL IN BROWSE BRW_AONR = gaok  */
  tidtemp.BEFATTNING:COLUMN-LABEL IN BROWSE BROWSE-1 = "vvv".
  RUN enable_UI.
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */

 _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE

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

 _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE

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
  ENABLE BROWSE-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
