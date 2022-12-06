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

{BRWSOK.I}
&Scoped-define NEW NEW
{KALKKATTEMP.I}
DEFINE SHARED VARIABLE kalkproch   AS HANDLE NO-UNDO. /* KALKAPP.P */
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_KAT

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES kalkkattemp

/* Definitions for BROWSE BRW_KAT                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_KAT kalkkattemp.TYP kalkkattemp.RADNR ~
kalkkattemp.VINAMN kalkkattemp.PRIS kalkkattemp.OPRIS 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KAT 
&Scoped-define QUERY-STRING-BRW_KAT FOR EACH kalkkattemp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_KAT OPEN QUERY BRW_KAT FOR EACH kalkkattemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_KAT kalkkattemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KAT kalkkattemp


/* Definitions for FRAME FRAME-A                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-BRW_KAT}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_KAT BTN_NY BTN_AND BTN_BORT BTN_AVS 

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

DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Bort":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 12 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_KAT FOR 
      kalkkattemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_KAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KAT WINDOW-1 _STRUCTURED
  QUERY BRW_KAT NO-LOCK DISPLAY
      kalkkattemp.TYP COLUMN-LABEL "Typ" FORMAT "X(12)":U
      kalkkattemp.RADNR COLUMN-LABEL "Sortering" FORMAT ">>>>>>9":U
      kalkkattemp.VINAMN COLUMN-LABEL "Kategori" FORMAT "X(256)":U
            WIDTH 45
      kalkkattemp.PRIS COLUMN-LABEL "Pris/Enh" FORMAT "->>>>>9.99":U
      kalkkattemp.OPRIS COLUMN-LABEL "Övertids!Pris" FORMAT "->>>>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 91.5 BY 8.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_KAT AT ROW 1.58 COL 1.5
     BTN_NY AT ROW 10.5 COL 26
     BTN_AND AT ROW 10.5 COL 40.75
     BTN_BORT AT ROW 10.5 COL 55.5
     BTN_AVS AT ROW 10.5 COL 79
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 93 BY 11.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: kalkkattemp T "?" NO-UNDO temp-db kalkkattemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Kategori"
         HEIGHT             = 11.04
         WIDTH              = 93
         MAX-HEIGHT         = 20.17
         MAX-WIDTH          = 98.75
         VIRTUAL-HEIGHT     = 20.17
         VIRTUAL-WIDTH      = 98.75
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
                                                                        */
/* BROWSE-TAB BRW_KAT 1 FRAME-A */
ASSIGN 
       BRW_KAT:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE
       BRW_KAT:COLUMN-RESIZABLE IN FRAME FRAME-A       = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KAT
/* Query rebuild information for BROWSE BRW_KAT
     _TblList          = "Temp-Tables.kalkkattemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.kalkkattemp.TYP
"kalkkattemp.TYP" "Typ" "X(12)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.kalkkattemp.RADNR
"kalkkattemp.RADNR" "Sortering" ">>>>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.kalkkattemp.VINAMN
"kalkkattemp.VINAMN" "Kategori" "X(256)" "character" ? ? ? ? ? ? no ? no no "45" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.kalkkattemp.PRIS
"kalkkattemp.PRIS" "Pris/Enh" "->>>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.kalkkattemp.OPRIS
"kalkkattemp.OPRIS" "Övertids!Pris" "->>>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_KAT */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_KAT
&Scoped-define SELF-NAME BRW_KAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_KAT WINDOW-1
ON VALUE-CHANGED OF BRW_KAT IN FRAME FRAME-A
DO:
  status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AND WINDOW-1
ON CHOOSE OF BTN_AND IN FRAME FRAME-A /* Ändra */
DO:
  RUN andra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS WINDOW-1
ON CHOOSE OF BTN_AVS IN FRAME FRAME-A /* Avsluta */
DO:
  RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-1
ON CHOOSE OF BTN_BORT IN FRAME FRAME-A /* Bort */
DO:
  RUN bort.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY WINDOW-1
ON CHOOSE OF BTN_NY IN FRAME FRAME-A /* Ny */
DO:
  RUN ny.
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
   {muswait.i}
   RUN hamtakalkkat_UI IN kalkproch (OUTPUT TABLE kalkkattemp, OUTPUT TABLE kalkkattyptemp).
   RUN enable_UI.   
   RUN openbdyn_UI IN brwproc[1] (INPUT "").
   {FRMSIZE.I}  
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
      (INPUT BRW_KAT:HANDLE IN FRAME {&FRAME-NAME}).
   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra WINDOW-1 
PROCEDURE andra :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}
   EMPTY TEMP-TABLE ekalkkattemp NO-ERROR. 
   CREATE ekalkkattemp.
   BUFFER-COPY kalkkattemp TO ekalkkattemp.
   RUN NYKAT.W (INPUT-OUTPUT TABLE ekalkkattemp). 
   {musarrow.i}
   IF musz = FALSE THEN DO: 
      FIND FIRST ekalkkattemp NO-LOCK NO-ERROR.
      RUN sparakalkat_UI IN kalkproch (INPUT TABLE ekalkkattemp).
      BUFFER-COPY ekalkkattemp TO kalkkattemp.
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(kalkkattemp)).              
      RUN openbdyn_UI IN brwproc[1] (INPUT "").
      RUN lastselectdyn_UI IN brwproc[1].                
   END.
   musz = FALSE.    
   EMPTY TEMP-TABLE ekalkkattemp NO-ERROR. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE bort WINDOW-1 
PROCEDURE bort :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   status-ok = BRW_KAT:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   MESSAGE "Vill du ta bort denna kategori ?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO 
   UPDATE answer AS LOGICAL.
   IF answer THEN DO: 
      {muswait.i}
      EMPTY TEMP-TABLE ekalkkattemp NO-ERROR. 
      CREATE ekalkkattemp.
      BUFFER-COPY kalkkattemp TO ekalkkattemp.
      RUN bortkalkkat_UI IN kalkproch (INPUT TABLE ekalkkattemp).
      DELETE kalkkattemp.
      RUN selnextprevrow_UI IN brwproc[1].
      RUN refreshbrw_UI IN brwproc[1].
      RUN lastselectdyn_UI IN brwproc[1].     
      EMPTY TEMP-TABLE ekalkkattemp NO-ERROR. 
      
      {musarrow.i}   
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
  ENABLE BRW_KAT BTN_NY BTN_AND BTN_BORT BTN_AVS 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ny WINDOW-1 
PROCEDURE ny :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}
   EMPTY TEMP-TABLE ekalkkattemp NO-ERROR. 
   RUN NYKAT.W (INPUT-OUTPUT TABLE ekalkkattemp). 
   {musarrow.i}
   IF musz = FALSE THEN DO: 
      FIND FIRST ekalkkattemp NO-LOCK NO-ERROR.
      RUN sparakalkat_UI IN kalkproch (INPUT TABLE ekalkkattemp).
      CREATE kalkkattemp.
      BUFFER-COPY ekalkkattemp TO kalkkattemp.
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(kalkkattemp)).              
      RUN openbdyn_UI IN brwproc[1] (INPUT "").
      RUN lastselectdyn_UI IN brwproc[1].          
   END.
   musz = FALSE.    
   EMPTY TEMP-TABLE ekalkkattemp NO-ERROR. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

