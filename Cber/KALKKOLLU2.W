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

&Scoped-define SHARED SHARED
{EXECLIN.I}
 

DEFINE VARIABLE KalkClasserStart AS HANDLE NO-UNDO.  

DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.         
DEFINE VARIABLE arrhjsum AS DECIMAL NO-UNDO.    
DEFINE VARIABLE str AS CHARACTER FORMAT "X(130)" NO-UNDO. 
DEFINE VARIABLE str2 AS CHARACTER FORMAT "X(130)" NO-UNDO.
DEFINE VARIABLE totalt AS DECIMAL NO-UNDO. 
DEFINE VARIABLE sumpris AS DECIMAL NO-UNDO.
DEFINE VARIABLE sumantal AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE vkonmtrlapph AS HANDLE NO-UNDO.
DEFINE VARIABLE iColumn                 AS INTEGER INITIAL 0.
DEFINE VARIABLE cColumn                 AS CHARACTER.


{KBETEMP.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-VINST
&Scoped-define BROWSE-NAME BRW_KBER

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES kbetemp

/* Definitions for BROWSE BRW_KBER                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_KBER kbetemp.KTYPKOD kbetemp.ARBKOD ~
kbetemp.LOPNR kbetemp.BENAMNING kbetemp.ENHET kbetemp.ANTAL kbetemp.F1 ~
kbetemp.F2 kbetemp.F3 kbetemp.F4 kbetemp.F5 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KBER 
&Scoped-define QUERY-STRING-BRW_KBER FOR EACH kbetemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_KBER OPEN QUERY BRW_KBER FOR EACH kbetemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_KBER kbetemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KBER kbetemp


/* Definitions for FRAME FRAME-VINST                                    */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-VINST ~
    ~{&OPEN-QUERY-BRW_KBER}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_KBER BTN_BORT FBTN_EXCEL BTN_AVS 

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

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_EXCEL 
     LABEL "Till Excel":L 
     SIZE 14 BY 1 TOOLTIP "Aktivera investering i Excel format".

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_KBER FOR 
      kbetemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_KBER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KBER WINDOW-2 _STRUCTURED
  QUERY BRW_KBER NO-LOCK DISPLAY
      kbetemp.KTYPKOD COLUMN-LABEL "Konstruktion" FORMAT "X(256)":U
            WIDTH 13
      kbetemp.ARBKOD COLUMN-LABEL "Arbkod" FORMAT "X(6)":U
      kbetemp.LOPNR COLUMN-LABEL "Löpnr" FORMAT ">>>9":U
      kbetemp.BENAMNING COLUMN-LABEL "Benämning" FORMAT "X(30)":U
      kbetemp.ENHET COLUMN-LABEL "Enhet" FORMAT "X(5)":U
      kbetemp.ANTAL COLUMN-LABEL "Antal" FORMAT ">>>>9.99":U
      kbetemp.F1 COLUMN-LABEL "Fält1" FORMAT "X(8)":U
      kbetemp.F2 COLUMN-LABEL "Fält2" FORMAT "X(8)":U
      kbetemp.F3 COLUMN-LABEL "Fält3" FORMAT "X(8)":U
      kbetemp.F4 COLUMN-LABEL "Fält4" FORMAT "X(8)":U
      kbetemp.F5 COLUMN-LABEL "Fält5" FORMAT "X(8)":U WIDTH 21.13
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 124 BY 25.5
         TITLE "Kopplade kalkyl-koder som inte finns i senaste kostnadskatalogen".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-VINST
     BRW_KBER AT ROW 1.75 COL 1
     BTN_BORT AT ROW 28.04 COL 81.88
     FBTN_EXCEL AT ROW 28.04 COL 95.5
     BTN_AVS AT ROW 28.04 COL 111
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 125 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: kbetemp T "?" NO-UNDO temp-db kbetemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Kopplade kalkyl-koder som inte finns i senaste kostnadskatalogen"
         COLUMN             = 36.25
         ROW                = 2.13
         HEIGHT             = 28.42
         WIDTH              = 125
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 125
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 125
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
/* BROWSE-TAB BRW_KBER 1 FRAME-VINST */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KBER
/* Query rebuild information for BROWSE BRW_KBER
     _TblList          = "Temp-Tables.kbetemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.kbetemp.KTYPKOD
"KTYPKOD" "Konstruktion" "X(256)" "character" ? ? ? ? ? ? no ? no no "13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.kbetemp.ARBKOD
"ARBKOD" "Arbkod" "X(6)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.kbetemp.LOPNR
"LOPNR" "Löpnr" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.kbetemp.BENAMNING
"BENAMNING" "Benämning" "X(30)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.kbetemp.ENHET
"ENHET" "Enhet" "X(5)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.kbetemp.ANTAL
"ANTAL" "Antal" ">>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.kbetemp.F1
"F1" "Fält1" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > Temp-Tables.kbetemp.F2
"F2" "Fält2" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > Temp-Tables.kbetemp.F3
"F3" "Fält3" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > Temp-Tables.kbetemp.F4
"F4" "Fält4" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > Temp-Tables.kbetemp.F5
"F5" "Fält5" ? "character" ? ? ? ? ? ? no ? no no "21.13" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_KBER */
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
   {BORTBRWPROC.I}
   IF VALID-HANDLE(KalkClasserStart) THEN DELETE PROCEDURE KalkClasserStart NO-ERROR.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-2
ON CHOOSE OF BTN_BORT IN FRAME FRAME-VINST /* Ta bort */
DO:        
   status-ok = BRW_KBER:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}  NO-ERROR.
   IF AVAILABLE kbetemp THEN DO:         
      MESSAGE "Vill Ni ta bort kalkylkod: " + kbetemp.ARBKOD + " " + STRING(kbetemp.LOPNR,Guru.Konstanter:varforetypchar[6]) + " på konstruktion " + kbetemp.KTYPKOD "?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Ta bort?" UPDATE svar AS LOGICAL.         
      IF svar THEN DO:                
         EMPTY TEMP-TABLE ekbetemp NO-ERROR. 
         CREATE ekbetemp.
         BUFFER-COPY kbetemp TO ekbetemp.
         DELETE kbetemp.        
         RUN selnextprevrow_UI IN brwproc[1].         
         RUN bort_UI IN vkonmtrlapph (INPUT TABLE ekbetemp).          
      END.      
      RUN openbdynspec_UI IN brwproc[1]. 
      RUN lastselectdyn_UI IN brwproc[1].     
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_EXCEL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_EXCEL WINDOW-2
ON CHOOSE OF FBTN_EXCEL IN FRAME FRAME-VINST /* Till Excel */
DO:
   
   RUN excel_UI.
   
   
   {musarrow.i}    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KBER
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
   IF VALID-HANDLE(vkonmtrlapph) THEN DELETE PROCEDURE vkonmtrlapph.
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
   EMPTY TEMP-TABLE kbetemp NO-ERROR. 
   RUN BerKalkUppKoll_UI IN KalkClasserStart (OUTPUT TABLE kbetemp).
   RUN enable_UI.   
   {FRMSIZE.I}          
   ASSIGN
   Guru.GlobalaVariabler:collefth = ?.
   Guru.GlobalaVariabler:colrighth = BTN_AVS:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = FBTN_EXCEL:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
   Guru.GlobalaVariabler:colrighth = BTN_BORT:HANDLE.      
   RUN buttcolh_UI IN framesizeh (INPUT Guru.GlobalaVariabler:collefth,INPUT Guru.GlobalaVariabler:colrighth,OUTPUT OPcollefth).
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
      (INPUT BRW_KBER:HANDLE IN FRAME {&FRAME-NAME}).
   IF NOT VALID-HANDLE(KalkClasserStart) THEN RUN Modules\Kalkyl\KalkClasserStart.P PERSISTENT SET KalkClasserStart.
   IF Guru.Konstanter:appcon THEN DO:
      RUN KALKKOAPP.P PERSISTENT SET vkonmtrlapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN KALKKOAPP.P PERSISTENT SET vkonmtrlapph.
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
  ENABLE BRW_KBER BTN_BORT FBTN_EXCEL BTN_AVS 
      WITH FRAME FRAME-VINST IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-VINST}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE excel_UI WINDOW-2 
PROCEDURE excel_UI :
/* -----------------------------------------------------------
  Purpose:      
  Parameters:  
  Notes:       
-------------------------------------------------------------*/
   
   CREATE "Excel.Application" chExcelApplication.
   chExcelApplication:Visible = TRUE.
   chWorkbook = chExcelApplication:Workbooks:Add() NO-ERROR.
   chWorkSheet = chExcelApplication:Sheets:Item(1) NO-ERROR.
   chWorkSheet:Columns("A"):ColumnWidth = 14 NO-ERROR.
   chWorkSheet:Columns("B"):ColumnWidth = 7 NO-ERROR.
   chWorkSheet:Columns("C"):ColumnWidth = 6 NO-ERROR.
   chWorkSheet:Columns("D"):ColumnWidth = 31 NO-ERROR.
   chWorkSheet:Columns("E"):ColumnWidth = 6 NO-ERROR.
   chWorkSheet:Columns("F"):ColumnWidth = 9 NO-ERROR.
   chWorkSheet:Columns("G"):ColumnWidth = 12 NO-ERROR.
   chWorkSheet:Columns("H"):ColumnWidth = 10 NO-ERROR.
   chWorkSheet:Columns("I"):ColumnWidth = 11 NO-ERROR.
   chWorkSheet:Columns("J"):ColumnWidth = 11 NO-ERROR.
   chWorkSheet:Columns("K"):ColumnWidth = 10 NO-ERROR.

   ASSIGN
   iColumn = 1
   cColumn = STRING(iColumn)
   cRange = "A" + cColumn.
   chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
   chWorkSheet:Range(cRange):FONT:ColorIndex = 11 NO-ERROR.
   {EXCELFEL.I} 
      
   chWorkSheet:Range(cRange):Value = "Kopplade kalkyl-koder som inte finns i senaste kostnadskatalogen " + STRING(TODAY) + " " + STRING(TIME,"HH:MM:SS") NO-ERROR.
   iColumn = iColumn + 2.

   
   cColumn = STRING(iColumn).
   cRange = "A" + cColumn.         
   chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
   chWorkSheet:Range(cRange):Value =  "KONSTRUKTION" NO-ERROR.
   cRange = "B" + cColumn.
   chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
   chWorkSheet:Range(cRange):Value = "ARBKOD" NO-ERROR.   
   cRange = "C" + cColumn.
   chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
   chWorkSheet:Range(cRange):Value = "LÖPNR"  NO-ERROR.
   cRange = "D" + cColumn.
   chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
   chWorkSheet:Range(cRange):Value = "BENÄMNING" NO-ERROR.
   cRange = "E" + cColumn.
   chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
   chWorkSheet:Range(cRange):Value = "ENHET" NO-ERROR.
   cRange = "F" + cColumn.
   chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
   chWorkSheet:Range(cRange):Value = "ANTAL" NO-ERROR.
   cRange = "G" + cColumn.
   chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
   chWorkSheet:Range(cRange):Value = "FÄLT1" NO-ERROR.
   cRange = "H" + cColumn.
   chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
   chWorkSheet:Range(cRange):Value = "FÄLT2" NO-ERROR.
   cRange = "I" + cColumn.
   chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
   chWorkSheet:Range(cRange):Value = "FÄLT3" NO-ERROR.
   cRange = "J" + cColumn.
   chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
   chWorkSheet:Range(cRange):Value = "FÄLT4" NO-ERROR.
   cRange = "K" + cColumn.
   chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
   chWorkSheet:Range(cRange):Value = "FÄLT5" NO-ERROR.
   iColumn = iColumn + 1.

   FOR EACH kbetemp NO-LOCK:
      iColumn = iColumn + 1.
      cColumn = STRING(iColumn).
      cRange = "A" + cColumn.         
      chWorkSheet:Range(cRange):Value =  kbetemp.KTYPKOD NO-ERROR.
      cRange = "B" + cColumn.
      chWorkSheet:Range(cRange):Value = kbetemp.ARBKOD NO-ERROR.
      cRange = "C" + cColumn.
      chWorkSheet:Range(cRange):Value = kbetemp.LOPNR  NO-ERROR.
      cRange = "D" + cColumn.
      chWorkSheet:Range(cRange):Value = kbetemp.BENAMNING NO-ERROR.
      cRange = "E" + cColumn.
      chWorkSheet:Range(cRange):Value = kbetemp.ENHET NO-ERROR.
      cRange = "F" + cColumn.
      chWorkSheet:Range(cRange):Value = kbetemp.ANTAL NO-ERROR.
      cRange = "G" + cColumn.
      chWorkSheet:Range(cRange):Value = kbetemp.F1 NO-ERROR.
      cRange = "H" + cColumn.
      chWorkSheet:Range(cRange):Value = kbetemp.F2 NO-ERROR.
      cRange = "I" + cColumn.
      chWorkSheet:Range(cRange):Value = kbetemp.F3 NO-ERROR.
      cRange = "J" + cColumn.
      chWorkSheet:Range(cRange):Value = kbetemp.F4 NO-ERROR.
      cRange = "K" + cColumn.
      chWorkSheet:Range(cRange):Value = kbetemp.F5 NO-ERROR.
      
   END.
   RELEASE OBJECT chWorkbook NO-ERROR.                   
   RELEASE OBJECT chExcelApplication NO-ERROR.           
   RELEASE OBJECT chPageBreak NO-ERROR.                  
   RELEASE OBJECT chCell NO-ERROR.                       
   RELEASE OBJECT chWorksheet NO-ERROR.                  
   RELEASE OBJECT chChart NO-ERROR.                      
   RELEASE OBJECT chWorksheetRange NO-ERROR.             
   RELEASE OBJECT cActiveCell NO-ERROR. 
   RELEASE OBJECT chWindow NO-ERROR.
   ASSIGN
   chWorkbook = ?  
   chExcelApplication = ?
   chPageBreak = ?
   chCell = ?
   chWorksheet = ?
   chChart = ?
   chWorksheetRange = ?
   cActiveCell = ?.
   chWindow = ?.
   
   
   {EXCELFEL.I}
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

