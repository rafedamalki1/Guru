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

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW

{GLOBVAR2DEL1.I}
DEFINE NEW SHARED VARIABLE tidlagproch AS HANDLE NO-UNDO. /* Procedure handle till TIDLAGAPP.P */
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.   
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE idtidlage AS CHARACTER NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(80)" NO-UNDO.

{TIDUTTTNEW.I}  
DEFINE NEW SHARED TEMP-TABLE tidslagtemp NO-UNDO
   FIELD AKTIVITET1  AS CHARACTER LABEL "Aktivitet"
   FIELD AKTIVITET2  AS CHARACTER LABEL "Aktivitet"
   FIELD DATUM1      AS DATE       FORMAT "99/99/99" INITIAL ?
   FIELD DATUM2      AS DATE       FORMAT "99/99/99" INITIAL ?
   FIELD TIDLAGE     AS CHARACTER 
   FIELD IDTIDLAG    AS CHARACTER 
   FIELD AUTOMATISKT AS LOGICAL    FORMAT "Auto/Man" INITIAL FALSE
   INDEX IDTIDLAGE IS PRIMARY IDTIDLAG TIDLAGE
   INDEX TIDLAGE TIDLAGE IDTIDLAG.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_TLAGE

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tidslagtemp

/* Definitions for BROWSE BRW_TLAGE                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_TLAGE tidslagtemp.TIDLAGE ~
tidslagtemp.AKTIVITET1 tidslagtemp.AKTIVITET2 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_TLAGE tidslagtemp.TIDLAGE 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_TLAGE tidslagtemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_TLAGE tidslagtemp
&Scoped-define QUERY-STRING-BRW_TLAGE FOR EACH tidslagtemp NO-LOCK ~
    BY tidslagtemp.TIDLAGE
&Scoped-define OPEN-QUERY-BRW_TLAGE OPEN QUERY BRW_TLAGE FOR EACH tidslagtemp NO-LOCK ~
    BY tidslagtemp.TIDLAGE.
&Scoped-define TABLES-IN-QUERY-BRW_TLAGE tidslagtemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_TLAGE tidslagtemp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_TLAGE FBTN_VPER FBTN_SKR BTN_NY BTN_AND ~
BTN_BORT BTN_AVB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AND 
     LABEL "Ändra":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE BUTTON FBTN_SKR 
     LABEL "Skriv ut":L 
     SIZE 14 BY 1.

DEFINE BUTTON FBTN_VPER 
     LABEL "Visa":L 
     SIZE 14 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_TLAGE FOR 
      tidslagtemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_TLAGE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_TLAGE WINDOW-1 _STRUCTURED
  QUERY BRW_TLAGE NO-LOCK DISPLAY
      tidslagtemp.TIDLAGE FORMAT "X(256)":U WIDTH 25
      tidslagtemp.AKTIVITET1 FORMAT "X(15)":U
      tidslagtemp.AKTIVITET2 FORMAT "X(15)":U
  ENABLE
      tidslagtemp.TIDLAGE
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 59 BY 20.04
         TITLE "Tidlägen".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_TLAGE AT ROW 1.83 COL 1.5
     FBTN_VPER AT ROW 6.88 COL 61.5
     FBTN_SKR AT ROW 7.96 COL 61.5
     BTN_NY AT ROW 22.33 COL 7.75
     BTN_AND AT ROW 22.33 COL 23.25
     BTN_BORT AT ROW 22.33 COL 38.88
     BTN_AVB AT ROW 22.33 COL 61.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 76.25 BY 22.79.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: tidslagtemp T "?" NO-UNDO temp-db tidslagtemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Tidlägen"
         HEIGHT             = 22.83
         WIDTH              = 76.63
         MAX-HEIGHT         = 25.75
         MAX-WIDTH          = 90.13
         VIRTUAL-HEIGHT     = 25.75
         VIRTUAL-WIDTH      = 90.13
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
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-A
   FRAME-NAME                                                           */
/* BROWSE-TAB BRW_TLAGE 1 FRAME-A */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_TLAGE
/* Query rebuild information for BROWSE BRW_TLAGE
     _TblList          = "Temp-Tables.tidslagtemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.tidslagtemp.TIDLAGE|yes"
     _FldNameList[1]   > Temp-Tables.tidslagtemp.TIDLAGE
"tidslagtemp.TIDLAGE" ? "X(256)" "character" ? ? ? ? ? ? yes ? no no "25" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.tidslagtemp.AKTIVITET1
"tidslagtemp.AKTIVITET1" ? "X(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.tidslagtemp.AKTIVITET2
"tidslagtemp.AKTIVITET2" ? "X(15)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_TLAGE */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_TLAGE
&Scoped-define SELF-NAME BRW_TLAGE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_TLAGE WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF BRW_TLAGE IN FRAME FRAME-A /* Tidlägen */
DO:
   RUN andra_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AND WINDOW-1
ON CHOOSE OF BTN_AND IN FRAME FRAME-A /* Ändra */
DO:
   RUN andra_UI. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avsluta */
DO:
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-1
ON CHOOSE OF BTN_BORT IN FRAME FRAME-A /* Ta bort */
DO:
   RUN bort_UI.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WINDOW-1
ON CHOOSE OF BTN_NY IN FRAME FRAME-A /* Ny */
DO:
   RUN ny_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FBTN_SKR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_SKR WINDOW-1
ON CHOOSE OF FBTN_SKR IN FRAME FRAME-A /* Skriv ut */
DO:    
   {muswait.i}
   skrivut = TRUE.  
   RUN SKRIVVAL.W (INPUT FALSE).
   IF musz = TRUE THEN DO:
      musz = FALSE.
      skrivut = FALSE.  
   END.
   ELSE DO:
      RUN ut_UI. 
   END.  
   skrivut = FALSE.
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


&Scoped-define SELF-NAME FBTN_VPER
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FBTN_VPER WINDOW-1
ON CHOOSE OF FBTN_VPER IN FRAME FRAME-A /* Visa */
DO:
   skrivut = FALSE.  
   RUN ut_UI.  
   {musarrow.i}   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
   IF VALID-HANDLE(tidlagproch) THEN DELETE PROCEDURE tidlagproch. 
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
   {ALLSTARTDYN.I}
   WINDOW-1:TITLE = Guru.Konstanter:gtidlk.
   tidslagtemp.TIDLAGE:LABEL IN BROWSE BRW_TLAGE = Guru.Konstanter:gtidlk.
   &Scoped-define BROWSE-NAME BRW_TLAGE
   BRW_TLAGE:TITLE = Guru.Konstanter:gtidlk.
   {muswait.i}
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
   tidslagtemp.TIDLAGE:READ-ONLY IN BROWSE BRW_TLAGE = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_TLAGE:HANDLE IN FRAME {&FRAME-NAME}).  
   tthandle = TEMP-TABLE tidslagtemp:HANDLE.   
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN DYNLADDATEMP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
         (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "TIDSLAGEN", INPUT "").
      RUN TIDLAGAPP.P PERSISTENT SET tidlagproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.
   END.
   ELSE DO:
      RUN DYNLADDATEMP.P (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "TIDSLAGEN", INPUT "").
      RUN TIDLAGAPP.P PERSISTENT SET tidlagproch.
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra_UI WINDOW-1 
PROCEDURE andra_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF AVAILABLE tidslagtemp THEN DO:
      
      idtidlage = tidslagtemp.IDTIDLAG.
      RUN TLAGENY.W (INPUT-OUTPUT idtidlage). 
      {musarrow.i}
      IF musz = FALSE THEN DO: 
         FIND FIRST tidslagtemp WHERE tidslagtemp.IDTIDLAG = idtidlage NO-LOCK NO-ERROR.
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(tidslagtemp)).
         RUN openbdynspec_UI IN brwproc[1].
         RUN lastselectdyn_UI IN brwproc[1].
      END.
      musz = FALSE.  
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bort_UI WINDOW-1 
PROCEDURE bort_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   IF AVAILABLE tidslagtemp THEN DO:
      MESSAGE "Vill du ta bort denna post ?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE tidslagtemp.TIDLAGE
      UPDATE answer AS LOGICAL.
      IF answer THEN DO TRANSACTION:
         {muswait.i}
         idtidlage = tidslagtemp.IDTIDLAG.
         RUN taborttidlag_UI IN tidlagproch (INPUT idtidlage).
         FIND FIRST tidslagtemp WHERE tidslagtemp.IDTIDLAG = idtidlage NO-LOCK NO-ERROR.
         DELETE tidslagtemp.
         RUN selnextprevrow_UI IN brwproc[1].
         RUN openbdynspec_UI IN brwproc[1].
         RUN lastselectdyn_UI IN brwproc[1].
         {musarrow.i}         
      END.
   END.   
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
  ENABLE BRW_TLAGE FBTN_VPER FBTN_SKR BTN_NY BTN_AND BTN_BORT BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ny_UI WINDOW-1 
PROCEDURE ny_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}
   
   idtidlage = "".
   RUN TLAGENY.W (INPUT-OUTPUT idtidlage).
   
   {musarrow.i}
   IF musz = FALSE THEN DO: 
      RUN openbdynspec_UI IN brwproc[1].
      FIND FIRST tidslagtemp WHERE tidslagtemp.IDTIDLAG = idtidlage NO-LOCK NO-ERROR.
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(tidslagtemp)).
      RUN lastselectdyn_UI IN brwproc[1].      
   END.
   musz = FALSE.    
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
  str = "===================.================.================".
  EMPTY TEMP-TABLE tidut NO-ERROR.    
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gtidlk)
   SUBSTRING(tidut.UT,30) = STRING(TODAY).
   CREATE tidut.
   CREATE tidut.              
   SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gtidlk).
   SUBSTRING(tidut.UT,21) = "AKTIVITET".                                
   SUBSTRING(tidut.UT,37) = "AKTIVITET".
   
   CREATE tidut.
   ASSIGN tidut.UT = str.
   OPEN QUERY lageq FOR EACH tidslagtemp USE-INDEX TIDLAGE NO-LOCK.
   GET FIRST lageq NO-LOCK.
   DO WHILE AVAILABLE(tidslagtemp):
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = STRING(tidslagtemp.TIDLAGE,"X(20)")
      SUBSTRING(tidut.UT,21) = tidslagtemp.AKTIVITET1                               
      SUBSTRING(tidut.UT,37) = tidslagtemp.AKTIVITET2. 
      GET NEXT lageq NO-LOCK.
   END.
   CLOSE QUERY lageq.       
   IF skrivut = FALSE THEN DO:
      {AVBGOM.I}
      RUN UTRAPP.W (INPUT Guru.Konstanter:gtidll).
      {AVBFRAM.I}
   END.
   ELSE DO:
      IF musz = TRUE THEN musz = FALSE.
      ELSE RUN EKLOGS.P.      
            
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

