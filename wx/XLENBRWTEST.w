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
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{ALLDEF.I}
{GLOBVAR2DEL1.I}
   DEFINE NEW SHARED VARIABLE borec AS RECID NO-UNDO.
   DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
   DEFINE NEW SHARED TEMP-TABLE tidut
      FIELD UT AS CHARACTER FORMAT "X(132)".   
   DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
   DEFINE VARIABLE stat AS LOGICAL NO-UNDO.
   DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
   DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
   DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
   
   

DEFINE {&NEW} {&SHARED} TEMP-TABLE akerkabtemp NO-UNDO
  FIELD VARDNR     AS INTEGER    FORMAT "->,>>>,>>9" INITIAL 0  LABEL "VÄRDERING NR"        
  FIELD BETECKNING AS CHARACTER  FORMAT "x(8)"                  LABEL "FASTIGHETSBETECKNING"
  FIELD KRONOR     AS INTEGER    FORMAT "->,>>>,>>9" INITIAL 0                   
  FIELD FASTPRIS   AS LOGICAL    FORMAT "JA/NEJ"     INITIAL NO                  
  FIELD L1         AS INTEGER    FORMAT ">>>9"       INITIAL 0  LABEL "LÄNGD L1"            
  FIELD FLKAB      AS LOGICAL    FORMAT "JA/NEJ"     INITIAL NO LABEL "Flera kablar"        
  FIELD L2         AS DECIMAL    FORMAT ">9.99"      INITIAL 0  LABEL "BREDD"
  FIELD AKERKABREC AS RECID
  INDEX VARDNR IS PRIMARY VARDNR BETECKNING.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES akerkabtemp

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 akerkabtemp.BETECKNING ~
akerkabtemp.VARDNR akerkabtemp.FASTPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 akerkabtemp.BETECKNING ~
akerkabtemp.VARDNR 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-1 akerkabtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-1 akerkabtemp
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH akerkabtemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH akerkabtemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 akerkabtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 akerkabtemp


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      akerkabtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 C-Win _STRUCTURED
  QUERY BROWSE-1 NO-LOCK DISPLAY
      akerkabtemp.BETECKNING FORMAT "X(8)":U
      akerkabtemp.VARDNR FORMAT "->,>>>,>>9":U
      akerkabtemp.FASTPRIS FORMAT "JA/NEJ":U
  ENABLE
      akerkabtemp.BETECKNING
      akerkabtemp.VARDNR
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 52.5 BY 12 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BROWSE-1 AT ROW 3.5 COL 13.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 16.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: akerkabtemp T "?" NO-UNDO temp-db akerkabtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 16
         WIDTH              = 80
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
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BROWSE-1 1 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _TblList          = "Temp-Tables.akerkabtemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.akerkabtemp.BETECKNING
"BETECKNING" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.akerkabtemp.VARDNR
"VARDNR" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[3]   = Temp-Tables.akerkabtemp.FASTPRIS
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
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


&Scoped-define BROWSE-NAME BROWSE-1
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
   {WIN_M_START.I}
   {ALLSTARTDYN.I}      
   CREATE akerkabtemp.   
   ASSIGN
   akerkabtemp.BETECKNING = "A"   
   akerkabtemp.VARDNR = 1.
   CREATE akerkabtemp.   
   ASSIGN
   akerkabtemp.BETECKNING = "B"   
   akerkabtemp.VARDNR = 1.
   CREATE akerkabtemp.   
   ASSIGN
   akerkabtemp.BETECKNING = "C"   
   akerkabtemp.VARDNR = 1.
   CREATE akerkabtemp.   
   ASSIGN
   akerkabtemp.BETECKNING = "D"   
   akerkabtemp.VARDNR = 1.
   CREATE akerkabtemp.   
   ASSIGN
   akerkabtemp.BETECKNING = "E"   
   akerkabtemp.VARDNR = 1.
   CREATE akerkabtemp.   
   ASSIGN
   akerkabtemp.BETECKNING = "F"   
   akerkabtemp.VARDNR = 1.
   CREATE akerkabtemp.   
   ASSIGN
   akerkabtemp.BETECKNING = "G"   
   akerkabtemp.VARDNR = 1.
   CREATE akerkabtemp.   
   ASSIGN
   akerkabtemp.BETECKNING = "H"   
   akerkabtemp.VARDNR = 1.
   CREATE akerkabtemp.   
   ASSIGN
   akerkabtemp.BETECKNING = "I"   
   akerkabtemp.VARDNR = 1.
   CREATE akerkabtemp.   
   ASSIGN
   akerkabtemp.BETECKNING = "J"   
   akerkabtemp.VARDNR = 1.
   CREATE akerkabtemp.   
   ASSIGN
   akerkabtemp.BETECKNING = "K"   
   akerkabtemp.VARDNR = 1.
   CREATE akerkabtemp.   
   ASSIGN
   akerkabtemp.BETECKNING = "L"   
   akerkabtemp.VARDNR = 1.
   CREATE akerkabtemp.   
   ASSIGN
   akerkabtemp.BETECKNING = "M"   
   akerkabtemp.VARDNR = 1.
   CREATE akerkabtemp.   
   ASSIGN
   akerkabtemp.BETECKNING = "N"   
   akerkabtemp.VARDNR = 1.
   CREATE akerkabtemp.   
   ASSIGN
   akerkabtemp.BETECKNING = "O"   
   akerkabtemp.VARDNR = 1.
   CREATE akerkabtemp.   
   /*RUN openbdyn_UI IN brwproc[1] (INPUT "").  */

  RUN enable_UI.
  {FRMSIZE.I} 
   
   APPLY "HOME" TO {&BROWSE-NAME}.
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW().   
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
      (INPUT BROWSE-1:HANDLE IN FRAME {&FRAME-NAME}).
   
   
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
  ENABLE BROWSE-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

