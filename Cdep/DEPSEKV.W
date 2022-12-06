&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-1


/* Temp-Table and Buffer definitions                                    */



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/05/02 - 12:41 pm

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
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{ANVPERS.I}
&Scoped-define NEW 
&Scoped-define SHARED SHARED
{DEPATEMP.I}
{DEPSEKTEMP.I}

DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vald_depa AS INTEGER NO-UNDO.

DEFINE VARIABLE depsekapph AS HANDLE NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE persrec AS RECID NO-UNDO.
DEFINE VARIABLE deprec AS RECID NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_DEPA

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES depsektemp anvandartemp

/* Definitions for BROWSE BRW_DEPA                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_DEPA depsektemp.anvandare ~
depsektemp.namn 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_DEPA 
&Scoped-define QUERY-STRING-BRW_DEPA FOR EACH depsektemp ~
      WHERE depsektemp.dep-nr = vald_depa NO-LOCK ~
    BY depsektemp.ANVANDARE INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_DEPA OPEN QUERY BRW_DEPA FOR EACH depsektemp ~
      WHERE depsektemp.dep-nr = vald_depa NO-LOCK ~
    BY depsektemp.ANVANDARE INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_DEPA depsektemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_DEPA depsektemp


/* Definitions for BROWSE BRW_PERS                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_PERS anvandartemp.ANVANDARE ~
anvandartemp.PERSONALKOD anvandartemp.AV-NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PERS 
&Scoped-define QUERY-STRING-BRW_PERS FOR EACH anvandartemp NO-LOCK ~
    BY anvandartemp.ANVANDARE INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_PERS OPEN QUERY BRW_PERS FOR EACH anvandartemp NO-LOCK ~
    BY anvandartemp.ANVANDARE INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_PERS anvandartemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PERS anvandartemp


/* Definitions for FRAME FRAME-A                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-A ~
    ~{&OPEN-QUERY-BRW_DEPA}~
    ~{&OPEN-QUERY-BRW_PERS}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-22 BRW_PERS BRW_DEPA BTN_SKAPA BTN_BORT ~
FILL-IN-ANV FILL-IN-ENH FILL-IN-NAMN BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-ANV FILL-IN-ENH FILL-IN-NAMN 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_BORT 
     IMAGE-UP FILE "BILDER\prev-u":U
     LABEL "" 
     SIZE 4 BY 1.21.

DEFINE BUTTON BTN_OK AUTO-END-KEY 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_SKAPA 
     IMAGE-UP FILE "BILDER\next-u":U
     LABEL "Button 1" 
     SIZE 4 BY 1.21.

DEFINE VARIABLE FILL-IN-ANV AS CHARACTER FORMAT "x(12)" 
     LABEL "Anv" 
     VIEW-AS FILL-IN 
     SIZE 13.88 BY .83.

DEFINE VARIABLE FILL-IN-ENH AS CHARACTER FORMAT "x(15)" 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .83.

DEFINE VARIABLE FILL-IN-NAMN AS CHARACTER FORMAT "x(25)" 
     LABEL "Namn" 
     VIEW-AS FILL-IN 
     SIZE 23.88 BY .83.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 54.5 BY 2.42
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_DEPA FOR 
      depsektemp SCROLLING.

DEFINE QUERY BRW_PERS FOR 
      anvandartemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_DEPA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_DEPA WINDOW-1 _STRUCTURED
  QUERY BRW_DEPA NO-LOCK DISPLAY
      depsektemp.anvandare COLUMN-LABEL "Behöriga!användare" FORMAT "X(18)":U
      depsektemp.namn COLUMN-LABEL "Namn" FORMAT "x(256)":U WIDTH 40
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 38.5 BY 24.

DEFINE BROWSE BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PERS WINDOW-1 _STRUCTURED
  QUERY BRW_PERS NO-LOCK DISPLAY
      anvandartemp.ANVANDARE COLUMN-LABEL "Användare" FORMAT "x(12)":U
      anvandartemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(7)":U
      anvandartemp.AV-NAMN COLUMN-LABEL "Namn" FORMAT "x(40)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING SIZE 54.5 BY 24.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_PERS AT ROW 1.75 COL 1.5
     BRW_DEPA AT ROW 1.75 COL 61
     BTN_SKAPA AT ROW 7.75 COL 56.38
     BTN_BORT AT ROW 11.58 COL 56.38
     FILL-IN-ANV AT ROW 26.96 COL 15.63 COLON-ALIGNED
     FILL-IN-ENH AT ROW 26.96 COL 41.88 COLON-ALIGNED
     FILL-IN-NAMN AT ROW 28.17 COL 15.63 COLON-ALIGNED
     BTN_OK AT ROW 28.17 COL 70.25
     BTN_AVB AT ROW 28.17 COL 85.25
     "Sök på:" VIEW-AS TEXT
          SIZE 7.75 BY .67 AT ROW 26.92 COL 2.5
     RECT-22 AT ROW 26.75 COL 1.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 98.75 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: anvandartemp T "?" NO-UNDO temp-db anvandartemp
      TABLE: depsektemp T "?" NO-UNDO temp-db depsektemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Sekretess för depå"
         HEIGHT             = 28.42
         WIDTH              = 99.38
         MAX-HEIGHT         = 28.42
         MAX-WIDTH          = 99.38
         VIRTUAL-HEIGHT     = 28.42
         VIRTUAL-WIDTH      = 99.38
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
/* BROWSE-TAB BRW_PERS RECT-22 FRAME-A */
/* BROWSE-TAB BRW_DEPA BRW_PERS FRAME-A */
ASSIGN 
       BRW_DEPA:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

ASSIGN 
       BRW_PERS:MAX-DATA-GUESS IN FRAME FRAME-A         = 300
       BRW_PERS:ALLOW-COLUMN-SEARCHING IN FRAME FRAME-A = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_DEPA
/* Query rebuild information for BROWSE BRW_DEPA
     _TblList          = "Temp-Tables.depsektemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Temp-Tables.depsektemp.ANVANDARE|yes"
     _Where[1]         = "Temp-Tables.depsektemp.dep-nr = vald_depa"
     _FldNameList[1]   > Temp-Tables.depsektemp.anvandare
"depsektemp.anvandare" "Behöriga!användare" "X(18)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.depsektemp.namn
"depsektemp.namn" "Namn" "x(256)" "character" ? ? ? ? ? ? no ? no no "40" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_DEPA */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PERS
/* Query rebuild information for BROWSE BRW_PERS
     _TblList          = "Temp-Tables.anvandartemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Temp-Tables.anvandartemp.ANVANDARE|yes"
     _FldNameList[1]   > Temp-Tables.anvandartemp.ANVANDARE
"anvandartemp.ANVANDARE" "Användare" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.anvandartemp.PERSONALKOD
"anvandartemp.PERSONALKOD" "Enhet/!Sign" "x(7)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.anvandartemp.AV-NAMN
"anvandartemp.AV-NAMN" "Namn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BRW_PERS */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME WINDOW-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL WINDOW-1 WINDOW-1
ON WINDOW-CLOSE OF WINDOW-1 /* Sekretess för depå */
DO:
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_DEPA
&Scoped-define SELF-NAME BRW_DEPA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_DEPA WINDOW-1
ON VALUE-CHANGED OF BRW_DEPA IN FRAME FRAME-A
DO:
  
  IF AVAILABLE depsektemp THEN DO:
     FIND FIRST anvandartemp WHERE anvandartemp.ANVANDARE = depsektemp.ANVANDARE NO-LOCK NO-ERROR.
     IF AVAILABLE anvandartemp THEN DO:
        RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(anvandartemp)).        
        RUN lastselectdyn_UI IN brwproc[1].      
     END.   
  END.     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avbryt */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT WINDOW-1
ON CHOOSE OF BTN_BORT IN FRAME FRAME-A
DO:
   FIND FIRST depatemp WHERE depatemp.DEP-NR = vald_depa 
   USE-INDEX DEP-NR NO-LOCK NO-ERROR.
   antal_valda = BRW_DEPA:NUM-SELECTED-ROWS.
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:           
      ASSIGN                        
      status-ok = BRW_DEPA:FETCH-SELECTED-ROW(antal_raknare)
      deprec = RECID(depsektemp). 
      MESSAGE "Vill du ta bort användare:" + depsektemp.ANVANDARE
      VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Borttag" UPDATE svar AS LOGICAL.         
      IF svar THEN DO TRANSACTION:
         IF depatemp.ANVANDARE = depsektemp.ANVANDARE THEN DO:
            MESSAGE "Du försöker ta bort den användare som är ansvarig för
            depå - " + depatemp.BENAMNING + ". Detta är ej tillåtet och borttag kommer ej att ske."
            VIEW-AS ALERT-BOX TITLE "Meddelande".
         END.
         ELSE DO:
            DELETE depsektemp.
            IF antal_raknare = antal_valda THEN RUN selnextprevrow_UI IN brwproc[2].
         END.   
      END.
      ELSE DO:
         musz = musz.
      END.      
      antal_raknare = antal_raknare + 1.   
   END.
   RUN setcolsortvar_UI IN brwproc[2] (INPUT " depsektemp.DEP-NR = '" +   STRING(vald_depa) + "'").
   RUN openbdynspec_UI IN brwproc[2].
   RUN lastselectdyn_UI IN brwproc[2].                         
   /*OPEN QUERY BRW_DEPA FOR EACH depsektemp WHERE depsektemp.DEP-NR = vald_depa 
   NO-LOCK BY depsektemp.ANVANDARE.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK WINDOW-1
ON CHOOSE OF BTN_OK IN FRAME FRAME-A /* Ok */
DO:    
   
   RUN sparpers_UI IN depsekapph (INPUT vald_depa,INPUT TABLE depsektemp).            
   APPLY "GO" TO BTN_OK IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK WINDOW-1
ON GO OF BTN_OK IN FRAME FRAME-A /* Ok */
DO:
  RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SKAPA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SKAPA WINDOW-1
ON CHOOSE OF BTN_SKAPA IN FRAME FRAME-A /* Button 1 */
DO:
   antal_valda = BRW_PERS:NUM-SELECTED-ROWS.
   antal_raknare = 1.
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = BRW_PERS:FETCH-SELECTED-ROW(antal_raknare). 
      FIND FIRST depsektemp WHERE depsektemp.DEP-NR = vald_depa AND
      depsektemp.ANVANDARE = anvandartemp.ANVANDARE /*USE-INDEX DEPNR*/
      NO-LOCK NO-ERROR.
      IF AVAILABLE depsektemp THEN DO:                       
         musz = musz.
      END.      
      ELSE DO TRANSACTION:
         CREATE depsektemp.
         ASSIGN
         depsektemp.DEP-NR = vald_depa
         depsektemp.ANVANDARE = anvandartemp.ANVANDARE
         depsektemp.NAMN = anvandartemp.AV-NAMN.
         RUN setlastrowid_UI IN brwproc[2] (INPUT ROWID(depsektemp)).              
      END.   
      antal_raknare = antal_raknare + 1.   
   END.
   
   RUN setcolsortvar_UI IN brwproc[2] (INPUT " depsektemp.DEP-NR = '" +   STRING(vald_depa) + "'").
   RUN openbdynspec_UI IN brwproc[2].
   RUN lastselectdyn_UI IN brwproc[2].     
   /*OPEN QUERY BRW_DEPA FOR EACH depsektemp WHERE depsektemp.DEP-NR = vald_depa 
   NO-LOCK BY depsektemp.ANVANDARE.*/
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
ON CLOSE OF THIS-PROCEDURE DO:
   {BORTBRWPROC.I}
   IF VALID-HANDLE(depsekapph) THEN DELETE PROCEDURE depsekapph.
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
   EMPTY TEMP-TABLE depsektemp NO-ERROR. 
   EMPTY TEMP-TABLE anvandartemp NO-ERROR. 
   RUN depsekhmt_UI IN depsekapph (OUTPUT TABLE depsektemp ,OUTPUT TABLE anvandartemp).                
   RUN openbdynspec_UI IN brwproc[1].    
   RUN setcolsortvar_UI IN brwproc[2] (INPUT " depsektemp.DEP-NR = '" +   STRING(vald_depa) + "'").
   RUN openbdynspec_UI IN brwproc[2].         
   FIND FIRST depatemp WHERE depatemp.DEP-NR = vald_depa USE-INDEX DEP-NR
   NO-LOCK NO-ERROR.
   {&WINDOW-NAME}:TITLE = "Sekretess för depå - " + depatemp.BENAMNING. 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/    

   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_PERS:HANDLE IN FRAME {&FRAME-NAME}).        
   RUN DYNBRW.P PERSISTENT SET brwproc[2]
      (INPUT BRW_DEPA:HANDLE IN FRAME {&FRAME-NAME}).
   
   RUN DYNARROW.P PERSISTENT SET brwproc[{&ARROWS}] 
      (INPUT BRW_PERS:HANDLE, INPUT BRW_DEPA:HANDLE ,
       INPUT ?, INPUT ? ,
       INPUT ?, INPUT ?).
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN DEPSEKAPPV.P PERSISTENT SET depsekapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN DEPSEKAPPV.P PERSISTENT SET depsekapph.
   END.      
   RUN addfillin_UI IN brwproc[1] 
      (INPUT FILL-IN-ANV:HANDLE IN FRAME {&FRAME-NAME}, INPUT "ANVANDARE").
   RUN addfillin_UI IN brwproc[1] 
      (INPUT FILL-IN-ENH:HANDLE IN FRAME {&FRAME-NAME}, INPUT "PERSONALKOD").
   RUN addfillin_UI IN brwproc[1] 
      (INPUT FILL-IN-NAMN:HANDLE IN FRAME {&FRAME-NAME}, INPUT "AV-NAMN").
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
  DISPLAY FILL-IN-ANV FILL-IN-ENH FILL-IN-NAMN 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE RECT-22 BRW_PERS BRW_DEPA BTN_SKAPA BTN_BORT FILL-IN-ANV FILL-IN-ENH 
         FILL-IN-NAMN BTN_OK BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo_UI WINDOW-1 
PROCEDURE repo_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   DEFINE INPUT PARAMETER brwvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER browrec AS RECID NO-UNDO.
   IF brwvar = 1 THEN DO:
      &Scoped-define BROWSE-NAME BRW_PERS
      {&BROWSE-NAME}:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
      REPOSITION {&BROWSE-NAME} TO RECID browrec NO-ERROR.  
   END.
   IF brwvar = 2 THEN DO:
      &Scoped-define BROWSE-NAME BRW_DEPA
      {&BROWSE-NAME}:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
      REPOSITION {&BROWSE-NAME} TO RECID browrec NO-ERROR.  
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

