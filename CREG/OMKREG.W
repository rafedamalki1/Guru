&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
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
{ALLDEF.I}
/* Local Variable Definitions ---                                       */
&Scoped-define NEW  
{GLOBVAR2DEL1.I}

DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE omkregproch AS HANDLE NO-UNDO. /* OMKREGAPP.P */
DEFINE VARIABLE str AS CHARACTER NO-UNDO.

&SCOPED-DEFINE NEW
&SCOPED-DEFINE SHARED
{OMKOSTNADTEMP.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_OMK

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES omkostnadtemp

/* Definitions for BROWSE BRW_OMK                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_OMK omkostnadtemp.OMRADE ~
omkostnadtemp.NAMN omkostnadtemp.ANSTALL omkostnadtemp.OMK1 ~
omkostnadtemp.OMK2 omkostnadtemp.OMK3 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_OMK omkostnadtemp.OMK1 ~
omkostnadtemp.OMK2 omkostnadtemp.OMK3 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_OMK omkostnadtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_OMK omkostnadtemp
&Scoped-define QUERY-STRING-BRW_OMK FOR EACH omkostnadtemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_OMK OPEN QUERY BRW_OMK FOR EACH omkostnadtemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_OMK omkostnadtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_OMK omkostnadtemp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-43 BRW_OMK FBTN_SKR FILL-IN_OMRS ~
FILL-IN_ORT BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_OMRS FILL-IN_ORT 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_SKR 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN_OMRS AS CHARACTER FORMAT "X(6)" 
     LABEL "Område" 
     VIEW-AS FILL-IN 
     SIZE 11.63 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_ORT AS CHARACTER FORMAT "x(40)" 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .83 NO-UNDO.

DEFINE RECTANGLE RECT-43
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 71 BY 1.21
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_OMK FOR 
      omkostnadtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_OMK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_OMK WINDOW-1 _STRUCTURED
  QUERY BRW_OMK NO-LOCK DISPLAY
      omkostnadtemp.OMRADE COLUMN-LABEL "Område" FORMAT "x(6)":U
      omkostnadtemp.NAMN COLUMN-LABEL "Benämning" FORMAT "X(20)":U
      omkostnadtemp.ANSTALL COLUMN-LABEL "Anställnings!form" FORMAT "X(8)":U
      omkostnadtemp.OMK1 COLUMN-LABEL "Omk 1" FORMAT "->>>>9.99":U
      omkostnadtemp.OMK2 COLUMN-LABEL "Omk 2" FORMAT "->>>>9.99":U
      omkostnadtemp.OMK3 COLUMN-LABEL "Omk 3" FORMAT "->>>>9.99":U
  ENABLE
      omkostnadtemp.OMK1
      omkostnadtemp.OMK2
      omkostnadtemp.OMK3
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 71 BY 13.38
         TITLE "Omkostnadstal".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_OMK AT ROW 1.5 COL 1.5
     FBTN_SKR AT ROW 4.46 COL 74.5
     FILL-IN_OMRS AT ROW 15.54 COL 23.13 COLON-ALIGNED
     FILL-IN_ORT AT ROW 15.54 COL 48.88 COLON-ALIGNED
     BTN_AVB AT ROW 15.54 COL 74.5
     "Sök på:" VIEW-AS TEXT
          SIZE 7.5 BY .83 AT ROW 15.54 COL 3.63
     RECT-43 AT ROW 15.33 COL 1.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 89.13 BY 16.04.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: omkostnadtemp T "?" NO-UNDO temp-db omkostnadtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Omkostnadstal"
         COLUMN             = 10.75
         ROW                = 6.33
         HEIGHT             = 16.04
         WIDTH              = 89.25
         MAX-HEIGHT         = 22.63
         MAX-WIDTH          = 115.25
         VIRTUAL-HEIGHT     = 22.63
         VIRTUAL-WIDTH      = 115.25
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
                                                                        */
/* BROWSE-TAB BRW_OMK RECT-43 FRAME-A */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_OMK
/* Query rebuild information for BROWSE BRW_OMK
     _TblList          = "Temp-Tables.omkostnadtemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.omkostnadtemp.OMRADE
"omkostnadtemp.OMRADE" "Område" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.omkostnadtemp.NAMN
"omkostnadtemp.NAMN" "Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.omkostnadtemp.ANSTALL
"omkostnadtemp.ANSTALL" "Anställnings!form" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.omkostnadtemp.OMK1
"omkostnadtemp.OMK1" "Omk 1" "->>>>9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.omkostnadtemp.OMK2
"omkostnadtemp.OMK2" "Omk 2" "->>>>9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.omkostnadtemp.OMK3
"omkostnadtemp.OMK3" "Omk 3" "->>>>9.99" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_OMK */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avsluta */
DO:
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKR WINDOW-1
ON CHOOSE OF FBTN_SKR IN FRAME FRAME-A /* Skriv ut */
DO:    
   {muswait.i}   
   RUN SKRIVVAL.W (INPUT FALSE).
   IF musz = TRUE THEN DO:
      musz = FALSE.      
   END.
   ELSE DO:
      RUN ut_UI. 
   END.     
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKR WINDOW-1
ON MOUSE-MENU-CLICK OF FBTN_SKR IN FRAME FRAME-A /* Skriv ut */
DO:
   RUN SIDLANGD.W.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_OMK
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
   RUN sparaomkreg_UI IN omkregproch (INPUT TABLE omkostnadtemp).
   IF VALID-HANDLE(omkregproch) THEN DELETE PROCEDURE omkregproch. 
   {BORTBRWPROC.I}
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
   RUN enable_UI. 
   {FRMSIZE.I}     
   RUN openbdynspec_UI IN brwproc[1].
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
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_OMK:HANDLE IN FRAME {&FRAME-NAME}).
   IF Guru.Konstanter:appcon THEN DO:
      RUN OMKREGAPP.P PERSISTENT SET omkregproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.         
   END.
   ELSE DO:
      RUN OMKREGAPP.P PERSISTENT SET omkregproch.         
   END.   
   RUN laddaomkreg_UI IN omkregproch (OUTPUT TABLE omkostnadtemp).
   RUN addfillin_UI IN brwproc[1] 
      (INPUT FILL-IN_ORT:HANDLE IN FRAME {&FRAME-NAME}, INPUT "NAMN").
   RUN addfillin_UI IN brwproc[1] 
      (INPUT FILL-IN_OMRS:HANDLE IN FRAME {&FRAME-NAME}, INPUT "OMRADE"). 
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
  DISPLAY FILL-IN_OMRS FILL-IN_ORT 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE RECT-43 BRW_OMK FBTN_SKR FILL-IN_OMRS FILL-IN_ORT BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW WINDOW-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ut_UI WINDOW-1 
PROCEDURE ut_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}
   {PRINTSTAENDE.I}
   PUT SKIP (2).   
   PUT PAGE-NUMBER AT 80 SKIP.              /*RAD 3*/
   FOR EACH omkostnadtemp:
      str = "".
      SUBSTRING(str,1,5) = omkostnadtemp.OMRADE.
      SUBSTRING(str,7,16) = omkostnadtemp.NAMN.
      SUBSTRING(str,24,4) = omkostnadtemp.ANSTALL.
      SUBSTRING(str,29,8) = STRING(omkostnadtemp.OMK1,">>>>9.99").
      SUBSTRING(str,38,8) = STRING(omkostnadtemp.OMK2,">>>>9.99").
      SUBSTRING(str,47,8) = STRING(omkostnadtemp.OMK3,">>>>9.99").                                   
      IF LINE-COUNTER = 1 THEN DO:
         /* PAGE.*/
         PUT SKIP (2).
         PUT PAGE-NUMBER AT 80 SKIP.
      END.         
      PUT UNFORMATTED str AT 6  SKIP.                                   
      IF LINE-COUNTER > PAGE-SIZE THEN DO:
         PAGE.
         PUT SKIP (2).
         PUT PAGE-NUMBER AT 80 SKIP.
      END.
   END.                     
   OUTPUT CLOSE.
   {musarrow.i}  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

