&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 08/19/96 -  8:22 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}

&Scoped-define NEW
{TIDPERS.I}
DEFINE SHARED VARIABLE perssokrec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1
&Scoped-define BROWSE-NAME BRW_URVAL

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tidpers

/* Definitions for BROWSE BRW_URVAL                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_URVAL tidpers.PERSONALKOD ~
tidpers.FORNAMN tidpers.EFTERNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_URVAL 
&Scoped-define QUERY-STRING-BRW_URVAL FOR EACH tidpers NO-LOCK
&Scoped-define OPEN-QUERY-BRW_URVAL OPEN QUERY BRW_URVAL FOR EACH tidpers NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_URVAL tidpers
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_URVAL tidpers


/* Definitions for DIALOG-BOX DIALOG-1                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DIALOG-1 ~
    ~{&OPEN-QUERY-BRW_URVAL}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-73 BRW_URVAL FILL-IN_SPERSONALKOD ~
FILL-IN_SFORNAMN FILL-IN_SEFTERNAMN BTN_OK BTN_AVSL 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_SPERSONALKOD FILL-IN_SFORNAMN ~
FILL-IN_SEFTERNAMN 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVSL AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "OK":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN_SEFTERNAMN AS CHARACTER FORMAT "x(25)" 
     LABEL "E-namn" 
     VIEW-AS FILL-IN 
     SIZE 10.5 BY .83.

DEFINE VARIABLE FILL-IN_SFORNAMN AS CHARACTER FORMAT "x(15)" 
     LABEL "F-namn" 
     VIEW-AS FILL-IN 
     SIZE 9.5 BY .83.

DEFINE VARIABLE FILL-IN_SPERSONALKOD AS CHARACTER FORMAT "x(5)" 
     LABEL "Enhet/Sign" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .83.

DEFINE RECTANGLE RECT-73
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 58 BY 2.42.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_URVAL FOR 
      tidpers SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_URVAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_URVAL DIALOG-1 _STRUCTURED
  QUERY BRW_URVAL NO-LOCK DISPLAY
      tidpers.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "X(5)":U
            WIDTH 7
      tidpers.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "X(15)":U
      tidpers.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "X(25)":U
            WIDTH 33
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 58 BY 16
         TITLE "Valda tidsedlar".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     BRW_URVAL AT ROW 3.5 COL 1.5
     FILL-IN_SPERSONALKOD AT ROW 21 COL 12.13 COLON-ALIGNED
     FILL-IN_SFORNAMN AT ROW 21 COL 26.63 COLON-ALIGNED
     FILL-IN_SEFTERNAMN AT ROW 21 COL 44.63 COLON-ALIGNED
     BTN_OK AT ROW 22.88 COL 30.5
     BTN_AVSL AT ROW 22.88 COL 45.5
     "~"Dubbel-Klicka på person vars tid skall ändras!" VIEW-AS TEXT
          SIZE 53 BY 1.5 AT ROW 1.5 COL 1.5
          FONT 17
     "Sök på:" VIEW-AS TEXT
          SIZE 12 BY .83 AT ROW 20.17 COL 3
     RECT-73 AT ROW 20 COL 1.5
     SPACE(0.87) SKIP(1.70)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Välj tidsedel".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db tidpers
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_URVAL RECT-73 DIALOG-1 */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_URVAL
/* Query rebuild information for BROWSE BRW_URVAL
     _TblList          = "Temp-Tables.tidpers"
     _Options          = "NO-LOCK "
     _FldNameList[1]   > Temp-Tables.tidpers.PERSONALKOD
"PERSONALKOD" "Enhet/!Sign" ? "" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.tidpers.FORNAMN
"FORNAMN" "Förnamn" ? "" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.tidpers.EFTERNAMN
"EFTERNAMN" "Efternamn" ? "" ? ? ? ? ? ? no ? no no "33" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_URVAL */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON GO OF FRAME DIALOG-1 /* Välj tidsedel */
DO:
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_URVAL
&Scoped-define SELF-NAME BRW_URVAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_URVAL DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF BRW_URVAL IN FRAME DIALOG-1 /* Valda tidsedlar */
DO:
   APPLY "CHOOSE" TO BTN_OK.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVSL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVSL DIALOG-1
ON CHOOSE OF BTN_AVSL IN FRAME DIALOG-1 /* Avbryt */
DO:
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* OK */
DO:
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() NO-ERROR.
   perssokrec = RECID(tidpers).        
   musz = FALSE.
   APPLY "WINDOW-CLOSE":U TO FRAME DIALOG-1.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SEFTERNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SEFTERNAMN DIALOG-1
ON ANY-KEY OF FILL-IN_SEFTERNAMN IN FRAME DIALOG-1 /* E-namn */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_SEFTERNAMN IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SEFTERNAMN DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_SEFTERNAMN IN FRAME DIALOG-1 /* E-namn */
DO:
   DEFINE VARIABLE posok AS CHARACTER NO-UNDO.   
   FILL-IN_SEFTERNAMN = INPUT FILL-IN_SEFTERNAMN.
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() NO-ERROR.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   perssokrec = RECID(tidpers).
   IF FILL-IN_SEFTERNAMN = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_SEFTERNAMN IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   posok = '*' + FILL-IN_SEFTERNAMN + '*'.
   FIND tidpers WHERE RECID(tidpers) = perssokrec NO-LOCK NO-ERROR.
   FIND NEXT tidpers WHERE tidpers.EFTERNAMN MATCHES posok       
   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.   
   IF NOT AVAILABLE tidpers THEN DO:
      FIND FIRST tidpers WHERE tidpers.EFTERNAMN MATCHES posok       
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidpers THEN DO:
         MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN_SEFTERNAMN IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
   END.
   IF AVAILABLE tidpers THEN DO:
      RUN repo_UI (INPUT RECID(tidpers)).
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() NO-ERROR.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SFORNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SFORNAMN DIALOG-1
ON ANY-KEY OF FILL-IN_SFORNAMN IN FRAME DIALOG-1 /* F-namn */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_SFORNAMN IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SFORNAMN DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_SFORNAMN IN FRAME DIALOG-1 /* F-namn */
DO:
   DEFINE VARIABLE posok AS CHARACTER NO-UNDO.   
   FILL-IN_SFORNAMN = INPUT FILL-IN_SFORNAMN.
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() NO-ERROR.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   perssokrec = RECID(tidpers).
   IF FILL-IN_SFORNAMN = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_SFORNAMN IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   posok = '*' + FILL-IN_SFORNAMN + '*'.   
   FIND tidpers WHERE RECID(tidpers) = perssokrec NO-LOCK NO-ERROR.
   FIND NEXT tidpers WHERE tidpers.FORNAMN MATCHES posok       
   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   
   IF NOT AVAILABLE tidpers THEN DO:
      FIND FIRST tidpers WHERE tidpers.FORNAMN MATCHES posok       
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.      
      IF NOT AVAILABLE tidpers THEN DO:
         MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN_SFORNAMN IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
   END.
   IF AVAILABLE tidpers THEN DO:
      RUN repo_UI (INPUT RECID(tidpers)).
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() NO-ERROR.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SPERSONALKOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SPERSONALKOD DIALOG-1
ON ANY-KEY OF FILL-IN_SPERSONALKOD IN FRAME DIALOG-1 /* Enhet/Sign */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_SPERSONALKOD IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SPERSONALKOD DIALOG-1
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_SPERSONALKOD IN FRAME DIALOG-1 /* Enhet/Sign */
DO:
   DEFINE VARIABLE posok AS CHARACTER NO-UNDO.
   FILL-IN_SPERSONALKOD = INPUT FILL-IN_SPERSONALKOD.
   status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() NO-ERROR.
   APPLY "VALUE-CHANGED" TO {&BROWSE-NAME}.
   perssokrec = RECID(tidpers).
   IF FILL-IN_SPERSONALKOD = '' THEN DO:
      MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
      APPLY "ENTRY" TO FILL-IN_SPERSONALKOD IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.      
   posok = '*' + FILL-IN_SPERSONALKOD + '*'.
   FIND tidpers WHERE RECID(tidpers) = perssokrec NO-LOCK NO-ERROR.
   FIND NEXT tidpers WHERE tidpers.PERSONALKOD MATCHES posok       
   USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   
   IF NOT AVAILABLE tidpers THEN DO:
      FIND FIRST tidpers WHERE tidpers.PERSONALKOD MATCHES posok       
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidpers THEN DO:
         MESSAGE "Det finns inget på sökbegreppet." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN_SPERSONALKOD IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.
   END.
   IF AVAILABLE tidpers THEN DO:
      RUN repo_UI (INPUT RECID(tidpers)).
      status-ok = {&BROWSE-NAME}:SELECT-FOCUSED-ROW() NO-ERROR.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
  RUN enable_UI.       
   {FRMSIZED.I} 
  {musarrow.i}
   {DIA_M_SLUT.I}
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY FILL-IN_SPERSONALKOD FILL-IN_SFORNAMN FILL-IN_SEFTERNAMN 
      WITH FRAME DIALOG-1.
  ENABLE RECT-73 BRW_URVAL FILL-IN_SPERSONALKOD FILL-IN_SFORNAMN 
         FILL-IN_SEFTERNAMN BTN_OK BTN_AVSL 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE repo_UI DIALOG-1 
PROCEDURE repo_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER browrec AS RECID NO-UNDO.
   {&BROWSE-NAME}:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
   REPOSITION {&BROWSE-NAME} TO RECID browrec NO-ERROR.  
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

