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
{BRWSOK.I}
&Scoped-define SHARED SHARED
{PHMT.I}
{GLOBVAR2DEL1.I}
{REGVAR.I}
{OMRTEMP.I}
/*{EGENBEN.I}*/
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE persrec AS RECID NO-UNDO.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_ORD

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES valperstemp personaltemp

/* Definitions for BROWSE BRW_ORD                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_ORD valperstemp.PERSONALKOD ~
valperstemp.VECKOSCHEMA valperstemp.FORNAMN valperstemp.EFTERNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_ORD valperstemp.PERSONALKOD 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_ORD valperstemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_ORD valperstemp
&Scoped-define OPEN-QUERY-BRW_ORD OPEN QUERY BRW_ORD FOR EACH valperstemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_ORD valperstemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_ORD valperstemp


/* Definitions for BROWSE BRW_PERS                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_PERS personaltemp.PERSONALKOD ~
personaltemp.AKTIV personaltemp.FORNAMN personaltemp.EFTERNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PERS personaltemp.PERSONALKOD 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_PERS personaltemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_PERS personaltemp
&Scoped-define OPEN-QUERY-BRW_PERS OPEN QUERY BRW_PERS FOR EACH personaltemp NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_PERS personaltemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PERS personaltemp


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_ORD BRW_PERS FILL-IN_SFORNAMN ~
FILL-IN_SEFTERNAMN 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_SFORNAMN FILL-IN_SEFTERNAMN 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE FILL-IN_SEFTERNAMN AS CHARACTER FORMAT "x(25)" 
     LABEL "Efternamn" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .91.

DEFINE VARIABLE FILL-IN_SFORNAMN AS CHARACTER FORMAT "x(15)" 
     LABEL "Förnamn" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_ORD FOR 
      valperstemp SCROLLING.

DEFINE QUERY BRW_PERS FOR 
      personaltemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_ORD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_ORD C-Win _STRUCTURED
  QUERY BRW_ORD NO-LOCK DISPLAY
      
   
   valperstemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(5)":U
            WIDTH 7
   /*   
   valperstemp.VECKOSCHEMA COLUMN-LABEL "Veckoschema" FORMAT "99":U
      valperstemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(15)":U
      */
      valperstemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(25)":U
            WIDTH 19
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 25.88 BY 10.09.

DEFINE BROWSE BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PERS C-Win _STRUCTURED
  QUERY BRW_PERS NO-LOCK DISPLAY
      personaltemp.PERSONALKOD COLUMN-LABEL "Enhet/!Sign" FORMAT "x(5)":U
            WIDTH 7
      personaltemp.AKTIV COLUMN-LABEL "Tid!skrivning" FORMAT "Aktiv/Inaktiv":U
      personaltemp.FORNAMN COLUMN-LABEL "Förnamn" FORMAT "x(15)":U
            WIDTH 13
      personaltemp.EFTERNAMN COLUMN-LABEL "Efternamn" FORMAT "x(25)":U
            WIDTH 17
  ENABLE
      personaltemp.PERSONALKOD
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 51 BY 8.45
         TITLE "Urvalsresultat" ROW-HEIGHT-CHARS .73.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BRW_ORD AT ROW 1.82 COL 54
     BRW_PERS AT ROW 2.09 COL 1
     FILL-IN_SFORNAMN AT ROW 11.64 COL 9.5 COLON-ALIGNED
     FILL-IN_SEFTERNAMN AT ROW 13.82 COL 63.38 COLON-ALIGNED
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
      TABLE: personaltemp T "?" NO-UNDO temp-db personaltemp
      TABLE: valperstemp T "?" NO-UNDO temp-db valperstemp
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
/* BROWSE-TAB BRW_ORD 1 DEFAULT-FRAME */
/* BROWSE-TAB BRW_PERS BRW_ORD DEFAULT-FRAME */
ASSIGN 
       BRW_PERS:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 10000.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_ORD
/* Query rebuild information for BROWSE BRW_ORD
     _TblList          = "Temp-Tables.valperstemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.valperstemp.PERSONALKOD
"valperstemp.PERSONALKOD" "Enhet/!Sign" ? "character" ? ? ? ? ? ? yes ? no no "7" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.valperstemp.VECKOSCHEMA
"valperstemp.VECKOSCHEMA" "Veckoschema" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.valperstemp.FORNAMN
"valperstemp.FORNAMN" "Förnamn" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.valperstemp.EFTERNAMN
"valperstemp.EFTERNAMN" "Efternamn" ? "character" ? ? ? ? ? ? no ? no no "19" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_ORD */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PERS
/* Query rebuild information for BROWSE BRW_PERS
     _TblList          = "Temp-Tables.personaltemp"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.personaltemp.PERSONALKOD
"personaltemp.PERSONALKOD" "Enhet/!Sign" ? "character" ? ? ? ? ? ? yes ? no no "7" yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.personaltemp.AKTIV
"personaltemp.AKTIV" "Tid!skrivning" "Aktiv/Inaktiv" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.personaltemp.FORNAMN
"personaltemp.FORNAMN" "Förnamn" ? "character" ? ? ? ? ? ? no ? no no "13" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.personaltemp.EFTERNAMN
"personaltemp.EFTERNAMN" "Efternamn" ? "character" ? ? ? ? ? ? no ? no no "17" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_PERS */
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


&Scoped-define BROWSE-NAME BRW_ORD
&Scoped-define SELF-NAME BRW_ORD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_ORD C-Win
ON ANY-PRINTABLE OF BRW_ORD IN FRAME DEFAULT-FRAME
DO:
   brwsok = LAST-EVENT:LABEL. 
   RUN hitta_UI.
   {musarrow.i}
   multitid = ETIME.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_ORD C-Win
ON START-SEARCH OF BRW_ORD IN FRAME DEFAULT-FRAME
DO:
   IF brwvar NE 1 THEN brwsortvar = 0.
   brwvar = 1.
   brwwh = BRW_ORD:CURRENT-COLUMN.   
   brwsok = LAST-EVENT:LABEL.     
   {muswait.i}   
   RUN opensok_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_PERS
&Scoped-define SELF-NAME BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PERS C-Win
ON ANY-PRINTABLE OF BRW_PERS IN FRAME DEFAULT-FRAME /* Urvalsresultat */
DO:
   brwsok = LAST-EVENT:LABEL. 
   RUN hitta_UI.
   {musarrow.i}
   multitid = ETIME.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PERS C-Win
ON ENTRY OF BRW_PERS IN FRAME DEFAULT-FRAME /* Urvalsresultat */
DO:
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PERS C-Win
ON START-SEARCH OF BRW_PERS IN FRAME DEFAULT-FRAME /* Urvalsresultat */
DO:
   IF brwvar NE 1 THEN brwsortvar = 0.
   brwvar = 1.
   brwwh = BRW_PERS:CURRENT-COLUMN.   
   brwsok = LAST-EVENT:LABEL.     
   {muswait.i}   
   RUN opensok_UI.
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SEFTERNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SEFTERNAMN C-Win
ON ANY-KEY OF FILL-IN_SEFTERNAMN IN FRAME DEFAULT-FRAME /* Efternamn */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_SEFTERNAMN IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SEFTERNAMN C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_SEFTERNAMN IN FRAME DEFAULT-FRAME /* Efternamn */
DO:
   RUN sok_UI (INPUT 3).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SFORNAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SFORNAMN C-Win
ON ANY-KEY OF FILL-IN_SFORNAMN IN FRAME DEFAULT-FRAME /* Förnamn */
DO:
   {TRYCKS.I}
   IF KEYFUNCTION(LASTKEY) = ("RETURN") THEN DO:
      APPLY "MOUSE-SELECT-DBLCLICK" TO FILL-IN_SFORNAMN IN FRAME {&FRAME-NAME}.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SFORNAMN C-Win
ON MOUSE-SELECT-DBLCLICK OF FILL-IN_SFORNAMN IN FRAME DEFAULT-FRAME /* Förnamn */
DO:
   RUN sok_UI (INPUT 1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_ORD
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
   CREATE PERSONALTEMP.
   ASSIGN PERSONALTEMP.PERSONALKOD = "AO".
   CREATE PERSONALTEMP.
   ASSIGN PERSONALTEMP.PERSONALKOD = "BB".
   CREATE valperstemp.
   ASSIGN valperstemp.EFTERNAMN = "AO".
   CREATE valperstemp.
   ASSIGN valperstemp.EFTERNAMN = "BB".
  RUN enable_UI.
  OPEN QUERY BRW_PERS FOR EACH PERSONALTEMP.
  OPEN QUERY BRW_ORD FOR EACH VALPERSTEMP.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY FILL-IN_SFORNAMN FILL-IN_SEFTERNAMN 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BRW_ORD BRW_PERS FILL-IN_SFORNAMN FILL-IN_SEFTERNAMN 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
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
   DEFINE INPUT PARAMETER varbrw AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER browrec AS RECID NO-UNDO.
   IF varbrw = 1 THEN DO:
      &Scoped-define BROWSE-NAME BRW_PERS
      {&BROWSE-NAME}:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
      REPOSITION {&BROWSE-NAME} TO RECID browrec NO-ERROR.  
   END.
   IF varbrw = 2 THEN DO:
      &Scoped-define BROWSE-NAME BRW_ORD
      {&BROWSE-NAME}:SET-REPOSITIONED-ROW(35,"ALWAYS") IN FRAME {&FRAME-NAME}.
      REPOSITION {&BROWSE-NAME} TO RECID browrec NO-ERROR.  
   END.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sok_UI WINDOW-1 
PROCEDURE sok_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   DEFINE INPUT PARAMETER sokvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE posok AS CHARACTER NO-UNDO.   
   IF sokvar = 1 THEN DO:
      FILL-IN_SFORNAMN = INPUT FRAME {&FRAME-NAME} FILL-IN_SFORNAMN.
      persrec = RECID(personaltemp).
      IF FILL-IN_SFORNAMN = '' THEN DO:
         MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN_SFORNAMN IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.      
      posok = '*' + FILL-IN_SFORNAMN + '*'.   
      FIND personaltemp WHERE RECID(personaltemp) = persrec NO-LOCK NO-ERROR.
      FIND NEXT personaltemp WHERE personaltemp.PERSONALKOD MATCHES posok       
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE personaltemp THEN DO:      
         FIND FIRST personaltemp WHERE personaltemp.PERSONALKOD MATCHES posok       
         USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.   
      END.      
      IF AVAILABLE personaltemp THEN DO:
      RUN repo_UI (INPUT 1,INPUT RECID(personaltemp)).
         status-ok = BRW_PERS:SELECT-FOCUSED-ROW() NO-ERROR.
      END.
   END.
   IF sokvar = 3 THEN DO:      
      FILL-IN_SEFTERNAMN = INPUT FILL-IN_SEFTERNAMN.
      persrec = RECID(valperstemp).
      IF FILL-IN_SEFTERNAMN = '' THEN DO:
         MESSAGE "Sökbegreppet kan inte vara blankt." VIEW-AS ALERT-BOX.
         APPLY "ENTRY" TO FILL-IN_SEFTERNAMN IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.      
      posok = '*' + FILL-IN_SEFTERNAMN + '*'.   
      FIND valperstemp WHERE RECID(valperstemp) = persrec NO-LOCK NO-ERROR.
      FIND NEXT valperstemp WHERE valperstemp.EFTERNAMN MATCHES posok       
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.   
      IF NOT AVAILABLE valperstemp THEN DO:      
         FIND FIRST valperstemp WHERE valperstemp.EFTERNAMN MATCHES posok       
         USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.   
      END.      
      IF AVAILABLE valperstemp THEN DO:
         RUN repo_UI (INPUT 2,INPUT RECID(valperstemp)).
         status-ok = BRW_ORD:SELECT-FOCUSED-ROW() NO-ERROR.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
   
   
