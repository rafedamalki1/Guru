&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
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
{ALLDEF.I}
&SCOPED-DEFINE NEW NEW
&SCOPED-DEFINE SHARED SHARED
{GLOBVAR2DEL1.I}
/*{EGENBVAR.I}*/

DEFINE NEW SHARED VARIABLE appcon AS LOGICAL NO-UNDO.

DEFINE VARIABLE brwh AS HANDLE NO-UNDO.
DEFINE VARIABLE bufh AS HANDLE NO-UNDO.
DEFINE VARIABLE colh AS HANDLE NO-UNDO.
DEFINE VARIABLE queh AS HANDLE NO-UNDO.
DEFINE VARIABLE tth AS HANDLE NO-UNDO.
DEFINE VARIABLE xpos AS INTEGER NO-UNDO.

DEFINE NEW SHARED VARIABLE fakkoproch AS HANDLE NO-UNDO. /* FAKKAPP.P */
DEFINE NEW SHARED TEMP-TABLE styrkfmtemp
   FIELD KUNDTYP     AS CHARACTER   FORMAT "X(20)":U
   FIELD KUNDFODTEXT AS CHARACTER   FORMAT "X(35)":U
   FIELD KUNDKONTO   AS CHARACTER   FORMAT "X(6)":U
   FIELD FAKTTYPTEXT AS CHARACTER   FORMAT "X(45)":U
   FIELD MOMSKOD     AS CHARACTER   FORMAT "X(6)":U
   FIELD INTAKTKONTO AS CHARACTER   FORMAT "X(6)":U
   FIELD MOTPART     AS CHARACTER   FORMAT "X(6)":U
   FIELD STYRKUND    AS RECID
   FIELD STYRINTAKT  AS RECID
   FIELD STYRMOT     AS RECID
   FIELD STYRMOMS    AS RECID
   INDEX KUNDTYP AS PRIMARY KUNDTYP.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_DYN

/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_DYN BTN_LADDA BTN_AVB RECT-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta" 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_LADDA 
     LABEL "Ladda" 
     SIZE 14 BY 1.5.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 98.5 BY 18.75.


/* Browse definitions                                                   */
DEFINE BROWSE BRW_DYN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_DYN C-Win _STRUCTURED
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 86 BY 13.5
         TITLE "Dynamisk så in i h.......".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BRW_DYN AT ROW 3 COL 8.13
     BTN_LADDA AT ROW 17.5 COL 44.13
     BTN_AVB AT ROW 17.75 COL 86
     RECT-1 AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 100 BY 19.71.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 19.71
         WIDTH              = 100
         MAX-HEIGHT         = 19.71
         MAX-WIDTH          = 100
         VIRTUAL-HEIGHT     = 19.71
         VIRTUAL-WIDTH      = 100
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
/* BROWSE-TAB BRW_DYN 1 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_DYN
/* Query rebuild information for BROWSE BRW_DYN
     _Query            is NOT OPENED
*/  /* BROWSE BRW_DYN */
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


&Scoped-define BROWSE-NAME BRW_DYN
&Scoped-define SELF-NAME BRW_DYN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_DYN C-Win
ON VALUE-CHANGED OF BRW_DYN IN FRAME DEFAULT-FRAME /* Dynamisk så in i h....... */
DO:
/*    DEFINE VARIABLE temph AS HANDLE NO-UNDO. */
/*    temph = bufh:BUFFER-FIELD(9).            */
/*    MESSAGE temph:BUFFER-VALUE.              */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
   APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_LADDA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_LADDA C-Win
ON CHOOSE OF BTN_LADDA IN FRAME DEFAULT-FRAME /* Ladda */
DO:
   RUN test_UI.  
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
   {ALLSTARTDYN.I}
/*    RUN FAKKAPP.P PERSISTENT SET fakkoproch.                      */
/*    RUN laddakfmtemp_UI IN fakkoproch (OUTPUT TABLE styrkfmtemp). */
   RUN enable_UI.
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI C-Win 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRWTEST.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_DYN:HANDLE IN FRAME {&FRAME-NAME}).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE creatett_UI C-Win 
PROCEDURE creatett_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  
  Notes:      
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER tablename AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE-HANDLE tthandle.
   
   DEFINE VARIABLE tempbufh AS HANDLE NO-UNDO.  
   DEFINE VARIABLE tempbufh2 AS HANDLE NO-UNDO.  
   DEFINE VARIABLE tempque AS HANDLE NO-UNDO.  

   CREATE TEMP-TABLE tthandle.
   tthandle:CREATE-LIKE(tablename).
   tthandle:TEMP-TABLE-PREPARE(LC(tablename) + "temp").
   tempbufh = tthandle:DEFAULT-BUFFER-HANDLE.
   CREATE BUFFER tempbufh2 FOR TABLE tablename.
   CREATE QUERY tempque.
   tempque:SET-BUFFERS(tempbufh2).
   tempque:QUERY-PREPARE("FOR EACH " + tablename + " NO-LOCK").
   tempque:QUERY-OPEN().
   tempque:GET-FIRST(NO-LOCK).
   DO WHILE tempque:QUERY-OFF-END = FALSE:
      tempbufh:BUFFER-CREATE.
      tempbufh:BUFFER-COPY(tempbufh2).
      tempque:GET-NEXT(NO-LOCK).
   END.
   tempque:QUERY-CLOSE().
   tempbufh2:BUFFER-RELEASE().
   DELETE OBJECT tempbufh2.
   DELETE OBJECT tempque.

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
  ENABLE BTN_LADDA BTN_AVB RECT-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE rowdisplayproc_UI C-Win 
PROCEDURE rowdisplayproc_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  
  Notes:      
------------------------------------------------------------------------------*/
   IF VALID-HANDLE(colh) THEN DO:
      colh:FORMAT = ">>9".      
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE test_UI C-Win 
PROCEDURE test_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  
  Notes:      
------------------------------------------------------------------------------*/
   brwh = BRW_DYN:HANDLE IN FRAME {&FRAME-NAME}.
   
   RUN creatett_UI (INPUT "STYRKUND", OUTPUT TABLE-HANDLE tth).
   /*    MESSAGE bufh:TABLE tth tth:NAME. */
/*    bufh = BUFFER OMRADETAB:HANDLE. */
/*    tth = TEMP-TABLE styrkfmtemp:HANDLE. */
   CREATE QUERY queh.
   queh:SET-BUFFERS(tth:DEFAULT-BUFFER-HANDLE).
   brwh:QUERY = queh.
   brwh:ADD-COLUMNS-FROM (tth:DEFAULT-BUFFER-HANDLE).
   brwh:ALLOW-COLUMN-SEARCHING = TRUE.
   RUN brwupdate_UI IN brwproc[1].   
/*    RUN setcolsortvar_UI IN brwproc[1] (INPUT "styrkfmtemp.STYRKUND NE ?") . */
   RUN openbdynspec_UI IN brwproc[1].
   RUN leftaligncol_UI (INPUT brwh:GET-BROWSE-COLUMN(1)).  

/*    queh:QUERY-PREPARE("FOR EACH " + bufh:TABLE + " WHERE styrkfmtemp.STYRINTAKT NE ? NO-LOCK"). */
/*    queh:QUERY-OPEN().                                                                           */
/*    queh:GET-FIRST(NO-LOCK).                                                                     */

/*    brwh:READ-ONLY = FALSE.           */   

/*    colh:READ-ONLY = FALSE.           */
/*    DEFINE VARIABLE tempstr AS CHARACTER NO-UNDO.                        */
/*    DEFINE VARIABLE tempstrall AS CHARACTER NO-UNDO.                     */
/*    DEFINE VARIABLE tempi AS INTEGER NO-UNDO.                            */
/*    DEFINE VARIABLE tempwidth AS INTEGER NO-UNDO.                        */
/*    tempi = 1.                                                           */
/*                                                                         */
/*    tempstr = "1,2,3,8".                                                 */
/* /*    tempstr = "1,4,5,6,9". */                                         */
/* /*    tempstr = "1,7,10". */                                            */
/* /*    tempstr = "1,4,5,11". */                                          */
/*    DO WHILE tempi LE brwh:NUM-COLUMNS:                                  */
/*       colh = brwh:GET-BROWSE-COLUMN(tempi).                             */
/*       colh:VISIBLE = FALSE.                                             */
/*       tempi = tempi + 1.                                                */
/*    END.                                                                 */
/*    tempi = 1.                                                           */
/*    tempwidth = 0.                                                       */
/*    DO WHILE tempi LE NUM-ENTRIES(tempstr, ","):                         */
/*       colh = brwh:GET-BROWSE-COLUMN(INTEGER(ENTRY(tempi,tempstr,","))). */
/*       colh:VISIBLE = TRUE.                                              */
/*       tempwidth = tempwidth + colh:WIDTH-PIXELS.                        */
/*       tempi = tempi + 1.                                                */
/*    END.                                                                 */
/*    xpos = brwh:X.                                                       */
/*    tempi = (brwh:WIDTH-PIXELS / 2) - ( (tempwidth + 50) / 2).           */
/*    MESSAGE xpos tempi.                                                  */
/*    brwh:WIDTH-PIXELS = tempwidth + 50.                                  */
/*    brwh:X = xpos + tempi.                                               */


/*    REPEAT:               */
/*                          */
/*       tempi = tempi + 1. */
/*    END.                  */
/*    colh = brwh:GET-BROWSE-COLUMN(2).                                                                      */
/*    colh:VISIBLE = FALSE.                                                                                  */
/*    colh = brwh:GET-BROWSE-COLUMN(3).                                                                      */
/*    colh:VISIBLE = FALSE.                                                                                  */
/*    brwh:TITLE = brwh:TITLE + "  " + STRING(brwh:NUM-VISIBLE-COLUMNS) + " av " + STRING(brwh:NUM-COLUMNS). */

/*    DEFINE VARIABLE tempi AS INTEGER NO-UNDO. */
/*    brwh:ADD-COLUMNS-FROM("omradetabtemp"). */
   
/*    colh = brwh:ADD-LIKE-COLUMN(bufh:BUFFER-FIELD(1)).                         */
/*    brwh:ADD-LIKE-COLUMN(bufh:BUFFER-FIELD(2)).                                */
/*    ON ROW-DISPLAY OF brwh PERSISTENT RUN rowdisplayproc_UI IN THIS-PROCEDURE. */
/*    colh:LABEL = "Mikael". */
/*    colh:WIDTH-CHARS = 20. */
   
/*    brwh:SENSITIVE = TRUE. */
   ENABLE ALL WITH FRAME {&FRAME-NAME}.
/*    MESSAGE brwh brwh:QUERY. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE leftaligncol_UI C-Win 
PROCEDURE leftaligncol_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  
  Notes:      
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER tempcolh AS HANDLE NO-UNDO.
   IF VALID-HANDLE(tempcolh) THEN DO:
      MESSAGE tempcolh:LABEL tempcolh:TYPE tempcolh:DATA-TYPE tempcolh:NAME  VIEW-AS ALERT-BOX.
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
