&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE respers NO-UNDO LIKE respers.


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
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE  TEMP-TABLE respers NO-UNDO   
   FIELD AONR AS CHARACTER 
   FIELD DELNR AS INTEGER 
   FIELD VECKONUMMER AS INTEGER
   FIELD DATUM AS DATE 
   FIELD DAG AS CHARACTER 
   FIELD START AS DECIMAL 
   FIELD SLUT AS DECIMAL
   FIELD PRIS AS DECIMAL
   FIELD PRISTYP AS CHARACTER 
   FIELD NATTRAKT AS LOGICAL FORMAT "JA/NEJ" LABEL "NATT TRAKT"
   FIELD OVERTIDUTTAG AS CHARACTER 
   FIELD BILFORARE AS LOGICAL
   FIELD ENFLERDAGS AS CHARACTER  
   FIELD TIDREC AS RECID
   FIELD KILOMETER AS DECIMAL
   INDEX RESPERS IS PRIMARY DATUM START SLUT ASCENDING.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_RESA-2

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES respers

/* Definitions for BROWSE BRW_RESA-2                                    */
&Scoped-define FIELDS-IN-QUERY-BRW_RESA-2 respers.DATUM respers.DAG ~
respers.AONR respers.DELNR respers.Kilometer respers.START respers.SLUT ~
respers.BILFORARE respers.NATTRAKT respers.OVERTIDUTTAG 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_RESA-2 respers.AONR ~
respers.DELNR respers.Kilometer respers.START respers.SLUT ~
respers.BILFORARE respers.NATTRAKT respers.OVERTIDUTTAG 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_RESA-2 respers
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_RESA-2 respers
&Scoped-define QUERY-STRING-BRW_RESA-2 FOR EACH respers NO-LOCK
&Scoped-define OPEN-QUERY-BRW_RESA-2 OPEN QUERY BRW_RESA-2 FOR EACH respers NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BRW_RESA-2 respers
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_RESA-2 respers


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_RESA-2 FOR 
      respers SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_RESA-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_RESA-2 C-Win _STRUCTURED
  QUERY BRW_RESA-2 NO-LOCK DISPLAY
      respers.DATUM COLUMN-LABEL "Datum" FORMAT "99/99/99":U
      respers.DAG COLUMN-LABEL "Dag" FORMAT "x(3)":U
      respers.AONR COLUMN-LABEL "Aonr" FORMAT "X(256)":U WIDTH 6
      respers.DELNR COLUMN-LABEL "Delnr" FORMAT ">99":U
      respers.Kilometer COLUMN-LABEL "Kilo-!meter" FORMAT ">>>>9":U
      respers.START COLUMN-LABEL "Start tid" FORMAT "99.99":U
      respers.SLUT COLUMN-LABEL "Slut tid" FORMAT "99.99":U
      respers.BILFORARE COLUMN-LABEL "Bil-!förare" FORMAT "Ja/Nej":U
      respers.NATTRAKT COLUMN-LABEL "Natt-!trakt." FORMAT "Ja/Nej":U
      respers.OVERTIDUTTAG COLUMN-LABEL "Övertids!uttag" FORMAT "X(1)":U
  ENABLE
      respers.AONR
      respers.DELNR
      respers.Kilometer
      respers.START
      respers.SLUT
      respers.BILFORARE
      respers.NATTRAKT
      respers.OVERTIDUTTAG
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-COLUMN-SCROLLING MULTIPLE SIZE 74.5 BY 12.96.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BRW_RESA-2 AT ROW 4.04 COL 6
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 101.13 BY 18.67.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: respers T "?" NO-UNDO temp-db respers
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 18.67
         WIDTH              = 101.13
         MAX-HEIGHT         = 18.67
         MAX-WIDTH          = 101.13
         VIRTUAL-HEIGHT     = 18.67
         VIRTUAL-WIDTH      = 101.13
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
/* BROWSE-TAB BRW_RESA-2 1 DEFAULT-FRAME */
/* SETTINGS FOR BROWSE BRW_RESA-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BRW_RESA-2:HIDDEN  IN FRAME DEFAULT-FRAME                = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_RESA-2
/* Query rebuild information for BROWSE BRW_RESA-2
     _TblList          = "Temp-Tables.respers"
     _Options          = "NO-LOCK"
     _FldNameList[1]   > Temp-Tables.respers.DATUM
"respers.DATUM" "Datum" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.respers.DAG
"respers.DAG" "Dag" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.respers.AONR
"respers.AONR" "Aonr" "X(256)" "character" ? ? ? ? ? ? yes ? no no "6" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.respers.DELNR
"respers.DELNR" "Delnr" ">99" "integer" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.respers.Kilometer
"respers.Kilometer" "Kilo-!meter" ">>>>9" "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.respers.START
"respers.START" "Start tid" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.respers.SLUT
"respers.SLUT" "Slut tid" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > Temp-Tables.respers.BILFORARE
"respers.BILFORARE" "Bil-!förare" "Ja/Nej" "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[9]   > Temp-Tables.respers.NATTRAKT
"respers.NATTRAKT" "Natt-!trakt." "Ja/Nej" "logical" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[10]   > Temp-Tables.respers.OVERTIDUTTAG
"respers.OVERTIDUTTAG" "Övertids!uttag" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_RESA-2 */
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


&Scoped-define BROWSE-NAME BRW_RESA-2
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
   CREATE respers.
   respers.AONR   = "123456".
   CREATE respers.
   respers.AONR   = "123457".
   CREATE respers.
   respers.AONR   = "123458".
   CREATE respers.
   respers.AONR   = "123459".
   &Scoped-define FORMATNAMN respers.AONR
   &Scoped-define BROWSE-NAME BRW_RESA-2
   {&FORMATNAMN}:WIDTH-CHARS IN BROWSE {&BROWSE-NAME} = 8.

  RUN enable_UI.
  OPEN QUERY BRW_RESA-2 FOR EACH respers USE-INDEX RESPERS.
   ENABLE BRW_RESA-2 WITH FRAME {&FRAME-NAME}.
   status-ok = BRW_RESA-2:SELECT-FOCUSED-ROW() NO-ERROR.      
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
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visa_UI C-Win 
PROCEDURE visa_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DISPLAY
   respers.AONR 
   respers.BILFORARE 
   respers.DELNR respers.Kilometer respers.SLUT respers.START   
   respers.NATTRAKT respers.OVERTIDUTTAG
      WITH BROWSE BRW_RESA-2.   
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

