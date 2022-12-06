&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1


/* Temp-Table and Buffer definitions                                    */




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 05/13/96 -  3:17 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}

DEFINE NEW SHARED VARIABLE brec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE prec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE borec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE hjvart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
{OMRTEMPW.I}
DEFINE NEW SHARED TEMP-TABLE kompbettemp
   FIELD PERSONALKOD AS CHARACTER    
   FIELD DATUM AS DATE  
   FIELD TIMMAR AS DECIMAL  
   FIELD ANVANDARE AS CHARACTER  
   FIELD ACCFORE AS DECIMAL  
   FIELD ACCEFTER AS DECIMAL 
   FIELD ACCKOMP AS DECIMAL
   FIELD FORE AS DECIMAL  /*VISNINGSFÄLT ACCFORE + ACCKOMP*/
   FIELD EFTER AS DECIMAL  /*VISNINGSFÄLT ACCEFTER + ACCKOMP*/
   FIELD FREC AS RECID
   INDEX PKOD IS PRIMARY PERSONALKOD DATUM
   INDEX DATUM DATUM PERSONALKOD
   INDEX TIMMAR TIMMAR 
   INDEX ACCFORE   ACCFORE   
   INDEX ACCEFTER  ACCEFTER  
   INDEX ANVANDARE ANVANDARE .
&Scoped-define NEW    NEW 
&Scoped-define SHARED SHARED
{ANVPERS.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_KOMPBET

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES kompbettemp

/* Definitions for BROWSE BRW_KOMPBET                                   */
&Scoped-define FIELDS-IN-QUERY-BRW_KOMPBET kompbettemp.PERSONALKOD ~
kompbettemp.DATUM kompbettemp.TIMMAR kompbettemp.FORE kompbettemp.EFTER ~
kompbettemp.ANVANDARE 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_KOMPBET 
&Scoped-define QUERY-STRING-BRW_KOMPBET FOR EACH kompbettemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_KOMPBET OPEN QUERY BRW_KOMPBET FOR EACH kompbettemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_KOMPBET kompbettemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_KOMPBET kompbettemp


/* Definitions for DIALOG-BOX DIALOG-1                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-48 BRW_KOMPBET BTN_NY BTN_AND ~
FILL-IN_PKODS FILL-IN_DATUMS BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_PKODS FILL-IN_DATUMS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AND 
     LABEL "Ändra":L 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_NY 
     LABEL "Ny":L 
     SIZE 12 BY 1.

DEFINE VARIABLE FILL-IN_DATUMS AS DATE FORMAT "99/99/99" 
     LABEL "Datum" 
     VIEW-AS FILL-IN 
     SIZE 12.88 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_PKODS AS CHARACTER FORMAT "X(6)" 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 11.88 BY .83 NO-UNDO.

DEFINE RECTANGLE RECT-48
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59 BY 1.21.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_KOMPBET FOR 
      kompbettemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_KOMPBET
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_KOMPBET DIALOG-1 _STRUCTURED
  QUERY BRW_KOMPBET NO-LOCK DISPLAY
      kompbettemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(5)":U
      kompbettemp.DATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
      kompbettemp.TIMMAR COLUMN-LABEL "Timmar" FORMAT "->>9.99":U
      kompbettemp.FORE COLUMN-LABEL "Kompsaldo!före" FORMAT "->>>>9.99":U
      kompbettemp.EFTER COLUMN-LABEL "Kompsaldo!efter" FORMAT "->>>>9.99":U
      kompbettemp.ANVANDARE COLUMN-LABEL "Användare" FORMAT "x(12)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 59 BY 21.5
         TITLE "Manuellt justerad kompsaldo".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BRW_KOMPBET AT ROW 1.33 COL 1.5 WIDGET-ID 100
     BTN_NY AT ROW 23.04 COL 23.5
     BTN_AND AT ROW 23.04 COL 23.5
     FILL-IN_PKODS AT ROW 24.71 COL 20.88 COLON-ALIGNED
     FILL-IN_DATUMS AT ROW 24.71 COL 40.38 COLON-ALIGNED
     BTN_AVB AT ROW 26.5 COL 46.5
     "Sök på" VIEW-AS TEXT
          SIZE 7 BY .83 AT ROW 24.71 COL 2.63
     RECT-48 AT ROW 24.5 COL 1.5
     SPACE(1.37) SKIP(1.95)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Manuell uppdatering kompsaldo".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: kompbettemp T "?" NO-UNDO temp-db kompbettemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE FRAME-NAME                                               */
/* BROWSE-TAB BRW_KOMPBET RECT-48 DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_KOMPBET
/* Query rebuild information for BROWSE BRW_KOMPBET
     _TblList          = "Temp-Tables.kompbettemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.kompbettemp.PERSONALKOD
"PERSONALKOD" "Enhet/!Sign" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.kompbettemp.DATUM
"DATUM" "Datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.kompbettemp.TIMMAR
"TIMMAR" "Timmar" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.kompbettemp.FORE
"FORE" "Kompsaldo!före" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.kompbettemp.EFTER
"EFTER" "Kompsaldo!efter" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.kompbettemp.ANVANDARE
"ANVANDARE" "Användare" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is NOT OPENED
*/  /* BROWSE BRW_KOMPBET */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AND
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AND DIALOG-1
ON CHOOSE OF BTN_AND IN FRAME DIALOG-1 /* Ändra */
DO:  
   RUN andra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avsluta */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NY
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NY DIALOG-1
ON CHOOSE OF BTN_NY IN FRAME DIALOG-1 /* Ny */
DO:
   RUN ny.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_DATUMS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_DATUMS DIALOG-1
ON ANY-KEY OF FILL-IN_DATUMS IN FRAME DIALOG-1 /* Datum */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_DATUMS IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_DATUMS DIALOG-1
ON LEAVE OF FILL-IN_DATUMS IN FRAME DIALOG-1 /* Datum */
DO:
   FILL-IN_DATUMS = INPUT FILL-IN_DATUMS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_DATUMS DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_DATUMS IN FRAME DIALOG-1 /* Datum */
DO:
   FILL-IN_DATUMS = INPUT FILL-IN_DATUMS.
   status-ok = BRW_KOMPBET:SELECT-FOCUSED-ROW() NO-ERROR.
   IF FILL-IN_DATUMS = ? THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_DATUMS IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   RUN sokurvaldyn_UI IN brwproc[1] (INPUT "DATUM", INPUT FILL-IN_DATUMS).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_PKODS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_PKODS DIALOG-1
ON ANY-KEY OF FILL-IN_PKODS IN FRAME DIALOG-1 /* Enhet/Sign */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_PKODS IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_PKODS DIALOG-1
ON LEAVE OF FILL-IN_PKODS IN FRAME DIALOG-1 /* Enhet/Sign */
DO:
   FILL-IN_PKODS = INPUT FILL-IN_PKODS.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_PKODS DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_PKODS IN FRAME DIALOG-1 /* Enhet/Sign */
DO:
   FILL-IN_PKODS = INPUT FILL-IN_PKODS.
   IF FILL-IN_PKODS = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_PKODS IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   RUN sokurvaldyn_UI IN brwproc[1] (INPUT "PERSONALKOD", INPUT FILL-IN_PKODS).   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_KOMPBET
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-1 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Add Trigger to equate WINDOW-CLOSE to END-ERROR                      */
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {DIA_M_START.I}
   {ALLSTARTDYN.I}
   RUN enable_UI.       
   {FRMSIZED.I}                          
   IF Guru.Konstanter:appcon THEN DO:
      RUN KOMPUTBETH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT 1,INPUT ?,INPUT ?,INPUT-OUTPUT TABLE kompbettemp).
   END.
   ELSE DO:
      RUN KOMPUTBETH.P 
      (INPUT 1,INPUT ?,INPUT ?,INPUT-OUTPUT TABLE kompbettemp).
   END.
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN ANVSKAP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT 2,INPUT "",INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
   END.
   ELSE DO:
      RUN ANVSKAP.P 
      (INPUT 2,INPUT "",INPUT-OUTPUT TABLE anvandartemp,INPUT-OUTPUT TABLE personaltemp).
   END.
   OPEN QUERY {&BROWSE-NAME} FOR EACH kompbettemp NO-LOCK BY kompbettemp.DATUM DESCENDING BY kompbettemp.PERSONALKOD  .
   BTN_AND:HIDDEN = TRUE.
   {musarrow.i}  
   APPLY "HOME" TO {&BROWSE-NAME}.
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() NO-ERROR.
   brec = RECID(kompbettemp).
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI DIALOG-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/ 
   /*kompbettemp.PERSONALKOD:READ-ONLY IN BROWSE BRW_KOMPBET = TRUE.*/
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_KOMPBET:HANDLE IN FRAME {&FRAME-NAME}).       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE andra DIALOG-1 
PROCEDURE andra :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME} NO-ERROR.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}. 
   IF NOT AVAILABLE kompbettemp THEN DO:
      MESSAGE "Du måste markera någon post innan du kan ändra" VIEW-AS ALERT-BOX.
      RETURN.
   END.   
   brec = RECID(kompbettemp).
   hjvart = "AND".
   
   RUN KOMPNY.W. 
   
   {musarrow.i}
   IF musz = FALSE THEN DO: 
      RUN openbdynspec_UI IN brwproc[1].
      FIND kompbettemp WHERE RECID(kompbettemp) = brec EXCLUSIVE-LOCK NO-ERROR.   
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(kompbettemp)).
      RUN lastselectdyn_UI IN brwproc[1].      
   END.
   musz = FALSE.      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-1  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME DIALOG-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-1  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN_PKODS FILL-IN_DATUMS 
      WITH FRAME DIALOG-1.
  ENABLE RECT-48 BRW_KOMPBET BTN_NY BTN_AND FILL-IN_PKODS FILL-IN_DATUMS 
         BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ny DIALOG-1 
PROCEDURE ny :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}
   DO TRANSACTION:   
      CREATE kompbettemp.
      brec = RECID(kompbettemp).
   END.   
   hjvart = "NYA".
   
   RUN KOMPNY.W.
   
   IF musz = TRUE THEN DO TRANSACTION:
      musz = FALSE.
      FIND kompbettemp WHERE RECID(kompbettemp) = brec EXCLUSIVE-LOCK NO-ERROR.   
      DELETE kompbettemp.     
   END.
   {musarrow.i}
   IF musz = FALSE THEN DO: 
      RUN openbdynspec_UI IN brwproc[1].
      FIND kompbettemp WHERE RECID(kompbettemp) = brec EXCLUSIVE-LOCK NO-ERROR.   
      RUN setlastrowid_UI IN brwproc[1] (INPUT ROWID(kompbettemp)).
      RUN lastselectdyn_UI IN brwproc[1].   
   END.
   musz = FALSE.    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

