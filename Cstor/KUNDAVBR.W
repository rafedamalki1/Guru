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
{GLOBVAR2DEL1.I}
DEFINE NEW SHARED VARIABLE bortrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE andra AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE valrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE specvar AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE startdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE starttid AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE totavbrott AS DECIMAL NO-UNDO.
DEFINE SHARED VARIABLE lspantal AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE hspantal AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE timmar100 AS DECIMAL NO-UNDO.
DEFINE VARIABLE timmar60 AS DECIMAL NO-UNDO.
DEFINE VARIABLE datum1 AS DATE NO-UNDO.
DEFINE VARIABLE datum2 AS DATE NO-UNDO.
DEFINE VARIABLE tid1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE tid2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE bortavbrott AS DECIMAL NO-UNDO.

DEFINE SHARED TEMP-TABLE bort_temp
   FIELD ATERDATUM AS DATE
   FIELD ATERKLOCKAN AS DECIMAL
   FIELD ANTALHSP AS INTEGER
   FIELD ANTALLSP AS INTEGER
   FIELD AVBROTTSTID AS DECIMAL
   FIELD AKTIVITET AS CHARACTER
   INDEX AVBROTT ATERDATUM ATERKLOCKAN ASCENDING.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_BORT

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES bort_temp

/* Definitions for BROWSE BRW_BORT                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_BORT bort_temp.ATERDATUM ~
bort_temp.ATERKLOCKAN bort_temp.ANTALHSP bort_temp.ANTALLSP ~
bort_temp.AVBROTTSTID bort_temp.AKTIVITET 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_BORT bort_temp.ATERDATUM 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_BORT bort_temp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_BORT bort_temp
&Scoped-define QUERY-STRING-BRW_BORT FOR EACH bort_temp NO-LOCK
&Scoped-define OPEN-QUERY-BRW_BORT OPEN QUERY BRW_BORT FOR EACH bort_temp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_BORT bort_temp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_BORT bort_temp


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_BORT BTN_NY BTN_AND BTN_BORT BTN_AVB ~
FILL-IN-STARTDAT FILL-IN-START FILL-IN-AVBROTT FILL-IN-HSPKUND ~
FILL-IN-LSPKUND 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-STARTDAT FILL-IN-START ~
FILL-IN-AVBROTT FILL-IN-HSPKUND FILL-IN-LSPKUND 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD klockan60 C-Win 
FUNCTION klockan60 RETURNS DECIMAL
  ( INPUT ber100 AS DECIMAL )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AND 
     LABEL "Ändra" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny" 
     SIZE 12 BY 1.

DEFINE VARIABLE FILL-IN-AVBROTT AS DECIMAL FORMAT ">>>>>>>9.99":U INITIAL 0 
     LABEL "Kundavbrottstid" 
      VIEW-AS TEXT 
     SIZE 14.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-HSPKUND AS INTEGER FORMAT ">>>>>":U INITIAL 0 
     LABEL "HSP-kunder" 
      VIEW-AS TEXT 
     SIZE 7.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LSPKUND AS INTEGER FORMAT ">>>>>>":U INITIAL 0 
     LABEL "LSP-kunder" 
      VIEW-AS TEXT 
     SIZE 7.25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-START AS DECIMAL FORMAT "99.99":U INITIAL 0 
     LABEL "Klockan" 
      VIEW-AS TEXT 
     SIZE 8 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-STARTDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Datum" 
      VIEW-AS TEXT 
     SIZE 9 BY 1 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_BORT FOR 
      bort_temp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_BORT C-Win _STRUCTURED
  QUERY BRW_BORT DISPLAY
      bort_temp.ATERDATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
      bort_temp.ATERKLOCKAN COLUMN-LABEL "Klockan" FORMAT "->>>>9.99":U
      bort_temp.ANTALHSP COLUMN-LABEL "HSP-kunder" FORMAT "->>>>>>9":U
      bort_temp.ANTALLSP COLUMN-LABEL "LSP-kunder" FORMAT "->>>>>>9":U
      bort_temp.AVBROTTSTID COLUMN-LABEL "Avbrottstid" FORMAT "->>>>9.99":U
      bort_temp.AKTIVITET COLUMN-LABEL "Aktivitet" FORMAT "X(20)":U
  ENABLE
      bort_temp.ATERDATUM
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 75 BY 10.75.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BRW_BORT AT ROW 5.38 COL 1.5
     BTN_NY AT ROW 16.54 COL 18.88
     BTN_AND AT ROW 16.54 COL 32.5
     BTN_BORT AT ROW 16.54 COL 46
     BTN_AVB AT ROW 16.54 COL 62.63
     FILL-IN-STARTDAT AT ROW 1.63 COL 16.13 COLON-ALIGNED
     FILL-IN-START AT ROW 1.63 COL 38.13 COLON-ALIGNED
     FILL-IN-AVBROTT AT ROW 2.96 COL 16.13 COLON-ALIGNED
     FILL-IN-HSPKUND AT ROW 4.33 COL 16.13 COLON-ALIGNED
     FILL-IN-LSPKUND AT ROW 4.33 COL 38.13 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 76.38 BY 16.83.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO TEMP-DB bort_temp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Beräkning av total kundavbrottstid"
         HEIGHT             = 16.71
         WIDTH              = 76.13
         MAX-HEIGHT         = 23.67
         MAX-WIDTH          = 94.75
         VIRTUAL-HEIGHT     = 23.67
         VIRTUAL-WIDTH      = 94.75
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
/* BROWSE-TAB BRW_BORT 1 DEFAULT-FRAME */
ASSIGN 
       FILL-IN-START:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

ASSIGN 
       FILL-IN-STARTDAT:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_BORT
/* Query rebuild information for BROWSE BRW_BORT
     _TblList          = "Temp-Tables.bort_temp"
     _FldNameList[1]   > Temp-Tables.bort_temp.ATERDATUM
"bort_temp.ATERDATUM" "Datum" ? "date" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.bort_temp.ATERKLOCKAN
"bort_temp.ATERKLOCKAN" "Klockan" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.bort_temp.ANTALHSP
"bort_temp.ANTALHSP" "HSP-kunder" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.bort_temp.ANTALLSP
"bort_temp.ANTALLSP" "LSP-kunder" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.bort_temp.AVBROTTSTID
"bort_temp.AVBROTTSTID" "Avbrottstid" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.bort_temp.AKTIVITET
"bort_temp.AKTIVITET" "Aktivitet" "X(20)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_BORT */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Beräkning av total kundavbrottstid */
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
ON WINDOW-CLOSE OF C-Win /* Beräkning av total kundavbrottstid */
DO:
  /* This event will close the window and terminate the procedure.  */
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_BORT
&Scoped-define SELF-NAME BRW_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_BORT C-Win
ON VALUE-CHANGED OF BRW_BORT IN FRAME DEFAULT-FRAME
DO:
   ASSIGN
   status-ok = BRW_BORT:SELECT-FOCUSED-ROW()
   bortrec = RECID(bort_temp).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AND C-Win
ON CHOOSE OF BTN_AND IN FRAME DEFAULT-FRAME /* Ändra */
DO:
   {muswait.i}
   
   FIND FIRST bort_temp NO-LOCK NO-ERROR.
   IF AVAILABLE bort_temp THEN DO:
      APPLY "VALUE-CHANGED" TO BRW_BORT IN FRAME {&FRAME-NAME}.
      ASSIGN
      musz = FALSE      
      andra = TRUE.
      RUN AVBROTT.W.
      IF musz = FALSE THEN DO:
         RUN berakna_UI.
         ASSIGN
         andra = FALSE
         specvar = TRUE.
         RUN refreshbrw_UI IN brwproc[1].
      END.   
      ELSE musz = FALSE.
   END.   
   ELSE DO:
      MESSAGE "Det finns inget att ändra."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.
   
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB C-Win
ON CHOOSE OF BTN_AVB IN FRAME DEFAULT-FRAME /* Avsluta */
DO:
   APPLY "CLOSE":U TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT C-Win
ON CHOOSE OF BTN_BORT IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
   FIND FIRST bort_temp WHERE NO-LOCK NO-ERROR.
   IF AVAILABLE bort_temp THEN DO:
      APPLY "VALUE-CHANGED" TO BRW_BORT IN FRAME {&FRAME-NAME}.
      MESSAGE "Vill du ta bort datum " + STRING(bort_temp.ATERDATUM) + " klockan " + STRING(bort_temp.ATERKLOCKAN) + "?"
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Ta bort?" UPDATE svar AS LOGICAL.         
      IF svar THEN DO: 
         specvar = TRUE.
         FIND bort_temp WHERE RECID(bort_temp) = bortrec EXCLUSIVE-LOCK NO-ERROR.     
         DELETE bort_temp.
         FIND FIRST bort_temp NO-LOCK NO-ERROR.
         IF AVAILABLE bort_temp THEN DO:
            RUN berakna_UI.
         END.
         ELSE DO:
            ASSIGN            
            FILL-IN-AVBROTT = 0
            FILL-IN-HSPKUND = 0
            FILL-IN-LSPKUND = 0.
            DISPLAY FILL-IN-AVBROTT FILL-IN-HSPKUND FILL-IN-LSPKUND WITH FRAME {&FRAME-NAME}.
         END.
         RUN refreshbrw_UI IN brwproc[1].
      END.
   END.   
   ELSE DO:
      MESSAGE "Det finns inget att ta bort."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY C-Win
ON CHOOSE OF BTN_NY IN FRAME DEFAULT-FRAME /* Ny */
DO:
   {muswait.i}
   
   ASSIGN         
   andra = FALSE
   musz = FALSE.
   RUN AVBROTT.W.
   IF musz = FALSE THEN DO:
      specvar = TRUE.
      RUN berakna_UI.
      RUN openbdynspec_UI IN brwproc[1].
      FIND FIRST bort_temp WHERE RECID(bort_temp) = bortrec NO-LOCK NO-ERROR.
      IF AVAILABLE bort_temp THEN DO:
         RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(bort_temp)).
         RUN lastselectdyn_UI IN brwproc[1].
      END.      
   END.
   ELSE musz = FALSE.   
   
   {musarrow.i}
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
   ASSIGN
   FILL-IN-AVBROTT = totavbrott
   FILL-IN-STARTDAT = startdatum
   FILL-IN-START = starttid
   FILL-IN-HSPKUND = hspantal
   FILL-IN-LSPKUND = lspantal.
   RUN openbdynspec_UI IN brwproc[1].
   RUN enable_UI.   
   {FRMSIZE.I}  
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
   bort_temp.ATERDATUM:READ-ONLY IN BROWSE BRW_BORT = TRUE.
   RUN DYNBRW.P PERSISTENT SET brwproc[1] 
      (INPUT BRW_BORT:HANDLE IN FRAME {&FRAME-NAME}).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE berakna_UI C-Win 
PROCEDURE berakna_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN
   totavbrott = 0.
   FIND FIRST bort_temp USE-INDEX AVBROTT EXCLUSIVE-LOCK NO-ERROR.
   ASSIGN
   datum1 = bort_temp.ATERDATUM
   tid1 = bort_temp.ATERKLOCKAN. 
   RUN TIDBER.P (INPUT startdatum, INPUT datum1, INPUT starttid,
   INPUT tid1, OUTPUT timmar100, OUTPUT timmar60).   
   ASSIGN
   bortavbrott = klockan60((timmar100 * (bort_temp.ANTALLSP + bort_temp.ANTALHSP)))
   bort_temp.AVBROTTSTID = bortavbrott.      
   
   totavbrott = totavbrott + (timmar100 * (bort_temp.ANTALLSP + bort_temp.ANTALHSP)).
   REPEAT:
      FIND NEXT bort_temp USE-INDEX AVBROTT EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE bort_temp THEN DO:
         ASSIGN
         datum2 = bort_temp.ATERDATUM
         tid2 = bort_temp.ATERKLOCKAN.
         RUN TIDBER.P (INPUT datum1, INPUT datum2, INPUT tid1,
         INPUT tid2, OUTPUT timmar100, OUTPUT timmar60).
         ASSIGN
         bortavbrott = klockan60((timmar100 * (bort_temp.ANTALLSP + bort_temp.ANTALHSP)))
         bort_temp.AVBROTTSTID = bortavbrott         
         totavbrott = totavbrott + (timmar100 * (bort_temp.ANTALLSP + bort_temp.ANTALHSP))
         datum1 = datum2
         tid1 = tid2.
      END.
      ELSE LEAVE.
   END.         
   ASSIGN
   FILL-IN-AVBROTT = klockan60(totavbrott)
   totavbrott = klockan60(totavbrott)
   FILL-IN-HSPKUND = hspantal
   FILL-IN-LSPKUND = lspantal.
   DISPLAY FILL-IN-AVBROTT FILL-IN-HSPKUND FILL-IN-LSPKUND WITH FRAME {&FRAME-NAME}.
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
  DISPLAY FILL-IN-STARTDAT FILL-IN-START FILL-IN-AVBROTT FILL-IN-HSPKUND 
          FILL-IN-LSPKUND 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BRW_BORT BTN_NY BTN_AND BTN_BORT BTN_AVB FILL-IN-STARTDAT 
         FILL-IN-START FILL-IN-AVBROTT FILL-IN-HSPKUND FILL-IN-LSPKUND 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION klockan60 C-Win 
FUNCTION klockan60 RETURNS DECIMAL
  ( INPUT ber100 AS DECIMAL ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   RETURN TRUNCATE(ber100,0) + ((ber100 - TRUNCATE(ber100,0)) / 100) * 60.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

