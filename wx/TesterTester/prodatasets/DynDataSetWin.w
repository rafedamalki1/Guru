&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME BrowseWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS BrowseWin 
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME BrowseFrame

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR BrowseWin AS WIDGET-HANDLE NO-UNDO.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME BrowseFrame
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 121.6 BY 16.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW BrowseWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Test Window for Dynamic DataSet display"
         HEIGHT             = 16
         WIDTH              = 121.6
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 147.6
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 147.6
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
/* SETTINGS FOR WINDOW BrowseWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME BrowseFrame
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(BrowseWin)
THEN BrowseWin:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BrowseWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BrowseWin BrowseWin
ON END-ERROR OF BrowseWin /* Test Window for Dynamic DataSet display */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BrowseWin BrowseWin
ON WINDOW-CLOSE OF BrowseWin /* Test Window for Dynamic DataSet display */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK BrowseWin 


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
  RUN enable_UI.
  RUN showDataSet.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI BrowseWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(BrowseWin)
  THEN DELETE WIDGET BrowseWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI BrowseWin  _DEFAULT-ENABLE
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
  VIEW FRAME BrowseFrame IN WINDOW BrowseWin.
  {&OPEN-BROWSERS-IN-QUERY-BrowseFrame}
  VIEW BrowseWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showDataSet BrowseWin 
PROCEDURE showDataSet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE hDataSet AS HANDLE NO-UNDO.
DEFINE VARIABLE hBrowse1 AS HANDLE NO-UNDO.
DEFINE VARIABLE hBrowse2 AS HANDLE NO-UNDO.
DEFINE VARIABLE hBrowse3 AS HANDLE NO-UNDO.
DEFINE VARIABLE hQuery1  AS HANDLE NO-UNDO.
DEFINE VARIABLE hQuery2  AS HANDLE NO-UNDO.
DEFINE VARIABLE hQuery3  AS HANDLE NO-UNDO.
DEFINE VARIABLE hBuffer1 AS HANDLE NO-UNDO.
DEFINE VARIABLE hBuffer2 AS HANDLE NO-UNDO.
DEFINE VARIABLE hBuffer3 AS HANDLE NO-UNDO.

/* RUN DynamicDataSet2.p (INPUT "Customer,Order,SalesRep",
                        INPUT "CustNum,OrderNum,SalesRep",
                        INPUT "CustNum,CustNum",
                        INPUT "< 10",
                        OUTPUT DATASET-HANDLE hDataSet).
                        */
RUN DynamicDataSet2.p (INPUT "Order,OrderLine,Item",
                        INPUT "OrderNum,LineNum,ItemNum",
                        INPUT "OrderNum,OrderNum",
                        INPUT "< 10",
                        OUTPUT DATASET-HANDLE hDataSet).

hBuffer1 = hDataSet:GET-BUFFER-HANDLE(1).
CREATE QUERY hQuery1.
hQuery1:ADD-BUFFER(hBuffer1).
hQuery1:QUERY-PREPARE("FOR EACH " + hBuffer1:NAME).
hQuery1:QUERY-OPEN().

CREATE BROWSE hBrowse1 ASSIGN
    QUERY = hQuery1
    FRAME = FRAME BrowseFrame:HANDLE
    HIDDEN = NO
    NO-VALIDATE = YES
    WIDTH = 120
    HEIGHT = 5
    SEPARATORS = YES
    SENSITIVE = YES.
hBrowse1:ADD-COLUMNS-FROM(hBuffer1:NAME).
hQuery2 = hDataSet:GET-RELATION(1):QUERY.
hQuery2:QUERY-OPEN().

CREATE BROWSE hBrowse2 ASSIGN
    QUERY = hQuery2
    FRAME = FRAME BrowseFrame:HANDLE
    HIDDEN = NO
    NO-VALIDATE = YES
    ROW = 6
    WIDTH = 120
    HEIGHT = 5
    SEPARATORS = YES
    SENSITIVE = YES.
hBuffer2 = hDataSet:GET-RELATION(1):CHILD-BUFFER.
hBrowse2:ADD-COLUMNS-FROM(hBuffer2:NAME).
hBuffer3 = hDataSet:GET-BUFFER-HANDLE(3).

CREATE QUERY hQuery3.
hQuery3:ADD-BUFFER(hBuffer3).
hQuery3:QUERY-PREPARE("FOR EACH " + hBuffer3:NAME).
hQuery3:QUERY-OPEN().

CREATE BROWSE hBrowse3 ASSIGN
    QUERY = hQuery3
    FRAME = FRAME BrowseFrame:HANDLE
    HIDDEN = NO
    NO-VALIDATE = YES
    ROW = 11
    WIDTH = 120
    HEIGHT = 5
    SEPARATORS = YES
    SENSITIVE = YES.
hBrowse3:ADD-COLUMNS-FROM(hBuffer3:NAME).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

