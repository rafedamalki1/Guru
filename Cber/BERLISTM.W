&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-2


/* Temp-Table and Buffer definitions                                    */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-2 
/*------------------------------------------------------------------------

  File: 
/*
   IF VALID-HANDLE(Guru.GlobalaVariabler:LastDialogh) THEN RUN Movetotop_UI IN Guru.GlobalaVariabler:LastDialogh.
   */
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
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{REGVAR.I}  
{ANMARKD.I}
{LISTDEF.I} 
{SOKDEF.I}
&Scoped-define SHARED SHARED
{KONVALTEMP.I}   
{HOPPSEK2W.I}
DEFINE SHARED VARIABLE btnbervalh AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE edmedd AS CHARACTER NO-UNDO.   
DEFINE SHARED VARIABLE valkonst AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE prisvar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.          
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valdelnr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE lin_recid AS RECID NO-UNDO.
DEFINE VARIABLE musz2 AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE arrhjsum AS DECIMAL NO-UNDO.    
DEFINE VARIABLE str AS CHARACTER FORMAT "X(130)" NO-UNDO. 
DEFINE VARIABLE str2 AS CHARACTER FORMAT "X(130)" NO-UNDO.
DEFINE VARIABLE totalt AS DECIMAL NO-UNDO. 
DEFINE VARIABLE sumpris AS DECIMAL NO-UNDO.
DEFINE VARIABLE sumantal AS INTEGER NO-UNDO.
DEFINE VARIABLE berlistmapph AS HANDLE NO-UNDO.
DEFINE VARIABLE mtrlrow AS ROWID NO-UNDO.
DEFINE VARIABLE utfil AS CHARACTER NO-UNDO.   
DEFINE TEMP-TABLE  htmlut
   FIELD UT AS CHARACTER FORMAT "X(132)".
DEFINE TEMP-TABLE mtrlut   
   FIELD ENR AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   INDEX ENR IS PRIMARY ENR ASCENDING.
DEFINE TEMP-TABLE mtrlinfo2  
   FIELD KTYPKOD AS CHARACTER
   FIELD ENR AS CHARACTER
   FIELD BENAMNING AS CHARACTER
   FIELD NUM AS INTEGER
   FIELD FRI1 AS INTEGER
   FIELD FRI2 AS CHARACTER
   FIELD FRI3 AS CHARACTER
   FIELD ANTAL AS DECIMAL
   FIELD PRIS AS DECIMAL
   FIELD LEVKOD AS CHARACTER
   FIELD UPPLAG AS INTEGER
   INDEX ENR IS PRIMARY ENR ASCENDING.
DEFINE TEMP-TABLE mtrl_temp2   
{MTRLTEMP2TT.I}
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-VINST
&Scoped-define BROWSE-NAME BRW_INFO

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES mtrlinfo2 mtrlut

/* Definitions for BROWSE BRW_INFO                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_INFO mtrlinfo2.Enr mtrlinfo2.Benamning ~
mtrlinfo2.KTYPKOD mtrlinfo2.FRI2 mtrlinfo2.FRI3 mtrlinfo2.ANTAL ~
mtrlinfo2.LEVKOD mtrlinfo2.PRIS mtrlinfo2.UPPLAG 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_INFO 
&Scoped-define QUERY-STRING-BRW_INFO FOR EACH mtrlinfo2 NO-LOCK
&Scoped-define OPEN-QUERY-BRW_INFO OPEN QUERY BRW_INFO FOR EACH mtrlinfo2 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_INFO mtrlinfo2
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_INFO mtrlinfo2


/* Definitions for BROWSE BRW_MTRL                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_MTRL mtrlut.Enr mtrlut.Benamning 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_MTRL 
&Scoped-define QUERY-STRING-BRW_MTRL FOR EACH mtrlut NO-LOCK
&Scoped-define OPEN-QUERY-BRW_MTRL OPEN QUERY BRW_MTRL FOR EACH mtrlut NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_MTRL mtrlut
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_MTRL mtrlut


/* Definitions for FRAME FRAME-VINST                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-VINST ~
    ~{&OPEN-QUERY-BRW_INFO}~
    ~{&OPEN-QUERY-BRW_MTRL}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_MTRL BTN_SPEC BRW_INFO BTN_AVS 

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

DEFINE BUTTON BTN_SPEC 
     LABEL "Visa" 
     SIZE 12 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_INFO FOR 
      mtrlinfo2 SCROLLING.

DEFINE QUERY BRW_MTRL FOR 
      mtrlut SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_INFO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_INFO WINDOW-2 _STRUCTURED
  QUERY BRW_INFO NO-LOCK DISPLAY
      mtrlinfo2.Enr COLUMN-LABEL "Enr" FORMAT "X(11)":U
      mtrlinfo2.Benamning COLUMN-LABEL "Benämning" FORMAT "x(256)":U
            WIDTH 30
      mtrlinfo2.KTYPKOD COLUMN-LABEL "Konstruktionskod" FORMAT "X(256)":U
            WIDTH 16
      mtrlinfo2.FRI2 COLUMN-LABEL "Id" FORMAT "X(256)":U WIDTH 14
      mtrlinfo2.FRI3 COLUMN-LABEL "Fri id" FORMAT "X(256)":U WIDTH 14
      mtrlinfo2.ANTAL COLUMN-LABEL "Antal" FORMAT "->>>>>>>>9":U
      mtrlinfo2.LEVKOD COLUMN-LABEL "Lev-id" FORMAT "X(8)":U
      mtrlinfo2.PRIS COLUMN-LABEL "Pris" FORMAT ">>>>9.99":U
      mtrlinfo2.UPPLAG COLUMN-LABEL "Upplag" FORMAT ">>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 116.5 BY 12.75.

DEFINE BROWSE BRW_MTRL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_MTRL WINDOW-2 _STRUCTURED
  QUERY BRW_MTRL NO-LOCK DISPLAY
      mtrlut.Enr COLUMN-LABEL "Enr" FORMAT "X(11)":U
      mtrlut.Benamning COLUMN-LABEL "Benämning" FORMAT "x(256)":U
            WIDTH 40
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 54.5 BY 11.25.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-VINST
     BRW_MTRL AT ROW 1.33 COL 29.5
     BTN_SPEC AT ROW 12.92 COL 50.75
     BRW_INFO AT ROW 14.33 COL 1.5
     BTN_AVS AT ROW 28 COL 103.88
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 117.25 BY 28.25.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: mtrlinfo2 T "?" NO-UNDO temp-db mtrlinfo2
      TABLE: mtrlut T "?" NO-UNDO temp-db mtrlut
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Använt materiel"
         HEIGHT             = 28.42
         WIDTH              = 117.25
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 123.63
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 123.63
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
/* SETTINGS FOR FRAME FRAME-VINST
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_MTRL 1 FRAME-VINST */
/* BROWSE-TAB BRW_INFO BTN_SPEC FRAME-VINST */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_INFO
/* Query rebuild information for BROWSE BRW_INFO
     _TblList          = "Temp-Tables.mtrlinfo2"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.mtrlinfo2.Enr
"mtrlinfo2.Enr" "Enr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.mtrlinfo2.Benamning
"mtrlinfo2.Benamning" "Benämning" "x(256)" "character" ? ? ? ? ? ? no ? no no "30" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.mtrlinfo2.KTYPKOD
"mtrlinfo2.KTYPKOD" "Konstruktionskod" "X(256)" "character" ? ? ? ? ? ? no ? no no "16" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.mtrlinfo2.FRI2
"mtrlinfo2.FRI2" "Id" "X(256)" "character" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.mtrlinfo2.FRI3
"mtrlinfo2.FRI3" "Fri id" "X(256)" "character" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.mtrlinfo2.ANTAL
"mtrlinfo2.ANTAL" "Antal" "->>>>>>>>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.mtrlinfo2.LEVKOD
"mtrlinfo2.LEVKOD" "Lev-id" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.mtrlinfo2.PRIS
"mtrlinfo2.PRIS" "Pris" ">>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.mtrlinfo2.UPPLAG
"mtrlinfo2.UPPLAG" "Upplag" ">>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_INFO */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_MTRL
/* Query rebuild information for BROWSE BRW_MTRL
     _TblList          = "Temp-Tables.mtrlut"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.mtrlut.Enr
"mtrlut.Enr" "Enr" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.mtrlut.Benamning
"mtrlut.Benamning" "Benämning" "x(256)" "character" ? ? ? ? ? ? no ? no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_MTRL */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-VINST
/* Query rebuild information for FRAME FRAME-VINST
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-VINST */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS WINDOW-2
ON CHOOSE OF BTN_AVS IN FRAME FRAME-VINST /* Avsluta */
DO:
   
   DELETE PROCEDURE Guru.GlobalaVariabler:LastDialogh NO-ERROR.  
   Guru.GlobalaVariabler:LastDialogh = ?.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SPEC
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SPEC WINDOW-2
ON CHOOSE OF BTN_SPEC IN FRAME FRAME-VINST /* Visa */
DO:
   RUN visa_UI.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_INFO
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-2 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   IF VALID-HANDLE(berlistmapph) THEN DELETE PROCEDURE berlistmapph.
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
   EMPTY TEMP-TABLE mtrlut NO-ERROR. 
   RUN skapalista_UI IN berlistmapph (INPUT valaonr,INPUT valomrade,INPUT valkonst,
                                      INPUT TABLE kon_val, OUTPUT musz2,OUTPUT TABLE mtrlut,OUTPUT TABLE mtrl_temp2).            
   str = "---------------------------------------------------------------------------------------".
   musz = musz2.
   IF musz = TRUE THEN DO:
      musz = FALSE.
      status-mus2 = CURRENT-WINDOW:LOAD-MOUSE-POINTER("ARROW").
      APPLY "WINDOW-CLOSE" TO {&WINDOW-NAME}. 
      LEAVE MAIN-BLOCK. 
   END.                 
   RUN setcolsortvar_UI  IN brwproc[1] (INPUT "").
   RUN openbdynspec_UI IN brwproc[1].
   RUN enable_UI.      
   IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN DO:      
      mtrlinfo2.PRIS:VISIBLE IN BROWSE BRW_INFO = FALSE.                         
   END.
   ELSE DO:
      ASSIGN
      mtrlinfo2.PRIS:VISIBLE IN BROWSE BRW_INFO = TRUE.            
   END.
   ON 'MOUSE-SELECT-DBLCLICK' OF BRW_MTRL PERSISTENT RUN visa_UI.
   ON 'VALUE-CHANGED' OF BRW_INFO PERSISTENT RUN andbrwb_UI IN THIS-PROCEDURE.            
   Guru.GlobalaVariabler:LastDialogh = THIS-PROCEDURE. 
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
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_MTRL:HANDLE IN FRAME FRAME-VINST).
   RUN DYNBRW.P PERSISTENT SET brwproc[2] 
      (INPUT BRW_INFO:HANDLE IN FRAME FRAME-VINST).
      
   RUN mouseselclick_UI IN brwproc[1] (INPUT FALSE).
   IF Guru.Konstanter:appcon THEN DO:
      RUN BERLISTMAPPU.P PERSISTENT SET berlistmapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN BERLISTMAPPU.P PERSISTENT SET berlistmapph.
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andbrwb_UI WINDOW-2 
PROCEDURE andbrwb_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/ 
   IF VALID-HANDLE(btnbervalh) THEN DO:
      RUN valpostbrwval_UI IN btnbervalh (INPUT mtrlinfo2.NUM).
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
  ENABLE BRW_MTRL BTN_SPEC BRW_INFO BTN_AVS 
      WITH FRAME FRAME-VINST IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-VINST}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Movetotop_UI WINDOW-2 
PROCEDURE Movetotop_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   {WIN_M_SLUT.I}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visa_UI WINDOW-2 
PROCEDURE visa_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   EMPTY TEMP-TABLE mtrlinfo2 NO-ERROR. 
   {muswait.i}
   ASSIGN
   antal_valda = BRW_MTRL:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:
      ASSIGN                                   
      status-ok = BRW_MTRL:FETCH-SELECTED-ROW(antal_raknare).
      IF AVAILABLE mtrlut THEN DO:
         mtrlrow = ROWID(mtrlut).                           
         RUN htmkonval2_UI IN berlistmapph (INPUT valaonr,INPUT valomrade,INPUT mtrlut.ENR,OUTPUT TABLE mtrlinfo2 APPEND).
      END.
      antal_raknare = antal_raknare + 1.   
   END. 
   {musarrow.i}
   FIND FIRST mtrlinfo2 NO-LOCK NO-ERROR.
   IF AVAILABLE mtrlinfo2 THEN DO:
      RUN setcolsortvar_UI  IN brwproc[2] (INPUT "").
      RUN openbdynspec_UI IN brwproc[2].
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

