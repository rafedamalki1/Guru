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

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}

DEFINE SHARED VARIABLE distnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE strproch AS HANDLE NO-UNDO. /*Handle till STORHMT.P*/
DEFINE SHARED VARIABLE distproch AS HANDLE NO-UNDO. /*Handle till DISTAPP.P*/
DEFINE SHARED VARIABLE natvar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE avbrytvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE kollvar AS INTEGER NO-UNDO.
DEFINE VARIABLE kollvar1 AS INTEGER NO-UNDO.
DEFINE VARIABLE kollvar2 AS INTEGER NO-UNDO.
DEFINE VARIABLE sparvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE artal AS INTEGER NO-UNDO.
{NATUPPLAGG.I}
&SCOPED-DEFINE NEW
&SCOPED-DEFINE SHARED SHARED 
{STORTEMP.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_NAT2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES natuppl2temp natuppl3temp natuppl4temp ~
leddata_temp

/* Definitions for BROWSE BRW_NAT2                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_NAT2 natuppl2temp.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_NAT2 natuppl2temp.NAMN 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_NAT2 natuppl2temp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_NAT2 natuppl2temp
&Scoped-define QUERY-STRING-BRW_NAT2 FOR EACH natuppl2temp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_NAT2 OPEN QUERY BRW_NAT2 FOR EACH natuppl2temp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_NAT2 natuppl2temp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_NAT2 natuppl2temp


/* Definitions for BROWSE BRW_NAT3                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_NAT3 natuppl3temp.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_NAT3 natuppl3temp.NAMN 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_NAT3 natuppl3temp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_NAT3 natuppl3temp
&Scoped-define QUERY-STRING-BRW_NAT3 FOR EACH natuppl3temp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_NAT3 OPEN QUERY BRW_NAT3 FOR EACH natuppl3temp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_NAT3 natuppl3temp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_NAT3 natuppl3temp


/* Definitions for BROWSE BRW_NAT4                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_NAT4 natuppl4temp.NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_NAT4 natuppl4temp.NAMN 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_NAT4 natuppl4temp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_NAT4 natuppl4temp
&Scoped-define QUERY-STRING-BRW_NAT4 FOR EACH natuppl4temp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_NAT4 OPEN QUERY BRW_NAT4 FOR EACH natuppl4temp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_NAT4 natuppl4temp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_NAT4 natuppl4temp


/* Definitions for BROWSE BRW_SPAN2                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_SPAN2 leddata_temp.NAMN ~
leddata_temp.LANGD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_SPAN2 leddata_temp.LANGD 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_SPAN2 leddata_temp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_SPAN2 leddata_temp
&Scoped-define QUERY-STRING-BRW_SPAN2 FOR EACH leddata_temp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_SPAN2 OPEN QUERY BRW_SPAN2 FOR EACH leddata_temp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_SPAN2 leddata_temp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_SPAN2 leddata_temp


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_NAT2 BRW_NAT3 BRW_NAT4 BRW_SPAN2 BTN_OK ~
BTN_AVB 

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
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_OK 
     LABEL "Spara" 
     SIZE 14 BY 1
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_NAT2 FOR 
      natuppl2temp SCROLLING.

DEFINE QUERY BRW_NAT3 FOR 
      natuppl3temp SCROLLING.

DEFINE QUERY BRW_NAT4 FOR 
      natuppl4temp SCROLLING.

DEFINE QUERY BRW_SPAN2 FOR 
      leddata_temp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_NAT2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_NAT2 C-Win _STRUCTURED
  QUERY BRW_NAT2 NO-LOCK DISPLAY
      natuppl2temp.NAMN COLUMN-LABEL "Namn" FORMAT "X(43)":U
  ENABLE
      natuppl2temp.NAMN
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-ROW-MARKERS SIZE 30 BY 5.

DEFINE BROWSE BRW_NAT3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_NAT3 C-Win _STRUCTURED
  QUERY BRW_NAT3 NO-LOCK DISPLAY
      natuppl3temp.NAMN COLUMN-LABEL "Namn" FORMAT "X(43)":U
  ENABLE
      natuppl3temp.NAMN
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-ROW-MARKERS SIZE 30 BY 5.

DEFINE BROWSE BRW_NAT4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_NAT4 C-Win _STRUCTURED
  QUERY BRW_NAT4 NO-LOCK DISPLAY
      natuppl4temp.NAMN COLUMN-LABEL "Namn" FORMAT "X(43)":U
  ENABLE
      natuppl4temp.NAMN
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 30 BY 5.

DEFINE BROWSE BRW_SPAN2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_SPAN2 C-Win _STRUCTURED
  QUERY BRW_SPAN2 DISPLAY
      leddata_temp.NAMN COLUMN-LABEL "Spänningsnivå" FORMAT "X(35)":U
      leddata_temp.LANGD COLUMN-LABEL "Antal / Längd" FORMAT ">>>>>>9":U
  ENABLE
      leddata_temp.LANGD
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 57.75 BY 12.17.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BRW_NAT2 AT ROW 2.58 COL 1.5
     BRW_NAT3 AT ROW 2.63 COL 32.25
     BRW_NAT4 AT ROW 2.63 COL 63.13
     BRW_SPAN2 AT ROW 8.04 COL 18.88
     BTN_OK AT ROW 11.46 COL 78
     BTN_AVB AT ROW 19.29 COL 78
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 92.5 BY 19.58.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: natuppl2temp T "?" NO-UNDO temp-db natuppl2temp
      TABLE: natuppl3temp T "?" NO-UNDO temp-db natuppl3temp
      TABLE: natuppl4temp T "?" NO-UNDO temp-db natuppl4temp
      TABLE: ? T "?" NO-UNDO temp-db leddata_temp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 19.5
         WIDTH              = 92.88
         MAX-HEIGHT         = 24.42
         MAX-WIDTH          = 98.38
         VIRTUAL-HEIGHT     = 24.42
         VIRTUAL-WIDTH      = 98.38
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
                                                                        */
/* BROWSE-TAB BRW_NAT2 1 DEFAULT-FRAME */
/* BROWSE-TAB BRW_NAT3 BRW_NAT2 DEFAULT-FRAME */
/* BROWSE-TAB BRW_NAT4 BRW_NAT3 DEFAULT-FRAME */
/* BROWSE-TAB BRW_SPAN2 BRW_NAT4 DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_NAT2
/* Query rebuild information for BROWSE BRW_NAT2
     _TblList          = "Temp-Tables.natuppl2temp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.natuppl2temp.NAMN
"natuppl2temp.NAMN" "Namn" "X(43)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_NAT2 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_NAT3
/* Query rebuild information for BROWSE BRW_NAT3
     _TblList          = "Temp-Tables.natuppl3temp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.natuppl3temp.NAMN
"natuppl3temp.NAMN" "Namn" "X(43)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_NAT3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_NAT4
/* Query rebuild information for BROWSE BRW_NAT4
     _TblList          = "Temp-Tables.natuppl4temp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.natuppl4temp.NAMN
"natuppl4temp.NAMN" "Namn" "X(43)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_NAT4 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_SPAN2
/* Query rebuild information for BROWSE BRW_SPAN2
     _TblList          = "Temp-Tables.leddata_temp"
     _FldNameList[1]   > Temp-Tables.leddata_temp.NAMN
"leddata_temp.NAMN" "Spänningsnivå" "X(35)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.leddata_temp.LANGD
"leddata_temp.LANGD" "Antal / Längd" ">>>>>>9" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_SPAN2 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
   APPLY "CLOSE":U TO THIS-PROCEDURE.
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


&Scoped-define BROWSE-NAME BRW_NAT2
&Scoped-define SELF-NAME BRW_NAT2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_NAT2 C-Win
ON VALUE-CHANGED OF BRW_NAT2 IN FRAME DEFAULT-FRAME
DO:
   ASSIGN
   status-ok = BRW_NAT2:SELECT-FOCUSED-ROW()
   kollvar = natuppl2temp.NATUPPLAGGID2
   kollvar1 = 0
   kollvar2 = 0.
   FIND FIRST natuppl3temp WHERE natuppl3temp.NATUPPLAGGID2 = natuppl2temp.NATUPPLAGGID2 
   NO-LOCK NO-ERROR.
   IF AVAILABLE natuppl3temp THEN DO:
      ASSIGN
      BRW_NAT3:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      OPEN QUERY BRW_NAT3 FOR EACH natuppl3temp WHERE 
      natuppl3temp.NATUPPLAGGID2 = natuppl2temp.NATUPPLAGGID2 NO-LOCK.       
      APPLY "VALUE-CHANGED" TO BRW_NAT3 IN FRAME {&FRAME-NAME}.
   END.
   ELSE DO:
      ASSIGN
      BRW_NAT3:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
      BRW_NAT4:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      RUN hamta_UI.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_NAT3
&Scoped-define SELF-NAME BRW_NAT3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_NAT3 C-Win
ON VALUE-CHANGED OF BRW_NAT3 IN FRAME DEFAULT-FRAME
DO:
   ASSIGN
   status-ok = BRW_NAT3:SELECT-FOCUSED-ROW()
   kollvar1 = natuppl3temp.NATUPPLAGGID3
   kollvar2 = 0.
   FIND FIRST natuppl4temp WHERE natuppl4temp.NATUPPLAGGID3 = natuppl3temp.NATUPPLAGGID3 
   NO-LOCK NO-ERROR.
   IF AVAILABLE natuppl4temp THEN DO:
      ASSIGN
      BRW_NAT4:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
      OPEN QUERY BRW_NAT4 FOR EACH natuppl4temp WHERE  
      natuppl4temp.NATUPPLAGGID3 = natuppl3temp.NATUPPLAGGID3 NO-LOCK.       
      APPLY "VALUE-CHANGED" TO BRW_NAT4 IN FRAME {&FRAME-NAME}.
   END.
   ELSE DO:
      ASSIGN
      BRW_NAT4:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
      RUN hamta_UI.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_NAT4
&Scoped-define SELF-NAME BRW_NAT4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_NAT4 C-Win
ON VALUE-CHANGED OF BRW_NAT4 IN FRAME DEFAULT-FRAME
DO:
   status-ok = BRW_NAT4:SELECT-FOCUSED-ROW().
   kollvar2 = natuppl4temp.NATUPPLAGGID4.
   RUN hamta_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:    
   RUN kollspar_UI.
   APPLY "CLOSE":U TO THIS-PROCEDURE.   
   RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK C-Win
ON CHOOSE OF BTN_OK IN FRAME DEFAULT-FRAME /* Spara */
DO:      
   {muswait.i}
   RUN grdledbtnok_UI IN distproch (INPUT distnr, INPUT artal, INPUT-OUTPUT TABLE leddata_temp).
   EMPTY TEMP-TABLE koll_temp NO-ERROR.    
   FOR EACH leddata_temp:
      CREATE koll_temp.
      ASSIGN
      koll_temp.SPANID = leddata_temp.SPANID
      koll_temp.LANGD = leddata_temp.LANGD. 
   END.    
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_NAT2
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   {BORTBRWPROC.I}
   RUN disable_UI.
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
   FIND stordistemp WHERE stordistemp.DISTRIKTID = distnr NO-LOCK NO-ERROR.
   artal = stordistemp.ARTAL.
   IF natvar = 2 THEN {&WINDOW-NAME}:TITLE = "Grunddata ledningar för distrikt " + stordistemp.NAMN + " årtal " + STRING(artal).     
   ELSE IF natvar = 3 THEN {&WINDOW-NAME}:TITLE = "Grunddata stationer för distrikt " + stordistemp.NAMN + " årtal " + STRING(artal).
   ELSE {&WINDOW-NAME}:TITLE = "Grunddata stationsapparater för distrikt " + stordistemp.NAMN + " årtal " + STRING(artal).
   RUN enable_UI.   
   {FRMSIZE.I}  
   OPEN QUERY BRW_NAT2 FOR EACH natuppl2temp WHERE natuppl2temp.NATUPPLAGGID1 = natvar NO-LOCK.       
   APPLY "VALUE-CHANGED" TO BRW_NAT2 IN FRAME {&FRAME-NAME}.   
   {musarrow.i}
   {WIN_M_SLUT.I}
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
   natuppl2temp.NAMN:READ-ONLY IN BROWSE BRW_NAT2 = TRUE.
   natuppl3temp.NAMN:READ-ONLY IN BROWSE BRW_NAT3 = TRUE.
   natuppl4temp.NAMN:READ-ONLY IN BROWSE BRW_NAT4 = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[2] 
      (INPUT BRW_NAT2:HANDLE IN FRAME {&FRAME-NAME}). 
   RUN DYNBRW.P PERSISTENT SET brwproc[3] 
      (INPUT BRW_NAT3:HANDLE IN FRAME {&FRAME-NAME}). 
   RUN DYNBRW.P PERSISTENT SET brwproc[4] 
      (INPUT BRW_NAT4:HANDLE IN FRAME {&FRAME-NAME}). 
   
   tthandle = TEMP-TABLE natuppl2temp:HANDLE.
   RUN laddatemp_UI IN strproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "NATUPPLAGG2").
   tthandle = TEMP-TABLE natuppl3temp:HANDLE.
   RUN laddatemp_UI IN strproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "NATUPPLAGG3").
   tthandle = TEMP-TABLE natuppl4temp:HANDLE.
   RUN laddatemp_UI IN strproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "NATUPPLAGG4").
   tthandle = TEMP-TABLE natuppkopptemp:HANDLE.
   RUN laddatemp_UI IN strproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "NATUPPKOPP").
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
  ENABLE BRW_NAT2 BRW_NAT3 BRW_NAT4 BRW_SPAN2 BTN_OK BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hamta_UI C-Win 
PROCEDURE hamta_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/ 
   RUN kollspar_UI.   
   EMPTY TEMP-TABLE spann_temp NO-ERROR. 
   EMPTY TEMP-TABLE leddata_temp NO-ERROR. 
   EMPTY TEMP-TABLE koll_temp NO-ERROR.            
   OPEN QUERY dq FOR EACH natuppkopptemp WHERE natuppkopptemp.NATUPPLAGGID1 = natvar AND
   natuppkopptemp.NATUPPLAGGID2 = kollvar NO-LOCK.
   GET FIRST dq NO-LOCK.
   DO WHILE AVAILABLE(natuppkopptemp):
      FIND FIRST spanningsnivtemp WHERE spanningsnivtemp.SPANID = natuppkopptemp.SPANID
      USE-INDEX SPANID NO-LOCK NO-ERROR.
      IF AVAILABLE spanningsnivtemp THEN DO:
         CREATE spann_temp.
         ASSIGN
         spann_temp.SPANID = spanningsnivtemp.SPANID
         spann_temp.NAMN = spanningsnivtemp.NAMN.
      END.
      GET NEXT dq NO-LOCK.
   END.
   CLOSE QUERY dq. 
   RUN grdledhamta_UI IN distproch (INPUT distnr, INPUT artal, INPUT natvar,
      INPUT kollvar, INPUT kollvar1, INPUT kollvar2, INPUT-OUTPUT TABLE leddata_temp,
      INPUT-OUTPUT TABLE spann_temp, INPUT-OUTPUT TABLE koll_temp).
   OPEN QUERY BRW_SPAN2 FOR EACH leddata_temp NO-LOCK.
   APPLY "ENTRY" TO BRW_SPAN2 IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE kollspar_UI C-Win 
PROCEDURE kollspar_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/ 
   sparvar = FALSE.
   FOR EACH leddata_temp:
      FIND FIRST koll_temp WHERE koll_temp.SPANID = leddata_temp.SPANID
      NO-LOCK NO-ERROR.
      IF leddata_temp.LANGD NE koll_temp.LANGD THEN DO:
         sparvar = TRUE.
      END.
   END.
   IF sparvar = TRUE THEN DO:
      MESSAGE "OBS! Vill du spara dina ändringar?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Spara ändringar?" UPDATE svar AS LOGICAL.         
      IF svar THEN DO:        
         APPLY "CHOOSE" TO BTN_OK IN FRAME {&FRAME-NAME}.
      END.
      ELSE DO:
         musz = musz.
      END. 
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

