&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win

/* Temp-Table and Buffer definitions                                    
DEFINE TEMP-TABLE utvaldfasttemp NO-UNDO LIKE utvaldfasttemp.
*/

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
/*
/*DEFINE SHARED VARIABLE hpApi AS HANDLE NO-UNDO.*/
DEFINE SHARED VARIABLE hpWinFunc AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE retvalkoll AS LOGICAL NO-UNDO.
*/
DEFINE VARIABLE hcur AS INTEGER NO-UNDO.
DEFINE VARIABLE retval AS INTEGER NO-UNDO.
DEFINE VARIABLE brwproc AS HANDLE EXTENT 25 NO-UNDO.
DEFINE VARIABLE apphandass AS HANDLE NO-UNDO.
DEFINE VARIABLE apphandklar AS HANDLE NO-UNDO.
DEFINE VARIABLE appprogok AS LOGICAL NO-UNDO.
&GLOBAL-DEFINE LEFT-BROWSE 1
&GLOBAL-DEFINE RIGHT-BROWSE 2
&GLOBAL-DEFINE ARROWS 3
DEFINE VARIABLE tthandle AS HANDLE NO-UNDO.
DEFINE VARIABLE ReturnValue AS INTEGER.

&Scoped-define NEW NEW 

DEFINE {&NEW} SHARED TEMP-TABLE utvaldfasttemp NO-UNDO
   FIELD AKTIV         AS LOGICAL
   FIELD ANVANDARE     AS CHARACTER
   FIELD AONR          AS CHARACTER
   FIELD ARTAL         AS INTEGER  LABEL "Årtal"
   FIELD BENAMNING     AS CHARACTER
   FIELD BESTID        AS CHARACTER
   FIELD DELNR         AS INTEGER
   FIELD EGETMTRL      AS LOGICAL
   FIELD EGETPRIS      AS LOGICAL
   FIELD FAKTOR        AS LOGICAL
   FIELD FASTPRIS      AS LOGICAL
   FIELD KALKANV       AS CHARACTER
   FIELD KALKNR        AS INTEGER
   FIELD KATAR         AS INTEGER
   FIELD OMRADE        AS CHARACTER
   FIELD PLANNR        AS CHARACTER
   FIELD SLUTDAG       AS CHARACTER
   FIELD SLUTVNR       AS INTEGER
   FIELD STARTDAG      AS CHARACTER
   FIELD STARTVNR      AS INTEGER
   FIELD TYP           AS INTEGER LABEL "Typ"
   FIELD TYPCHAR       AS CHARACTER LABEL "Typ"
   FIELD STATUSNIV     AS CHARACTER
   INDEX KALKNR IS PRIMARY TYP KALKNR
   INDEX KALKSP OMRADE TYP KALKNR 
   INDEX PLAN PLANNR ARTAL
   INDEX AONR AONR DELNR
   INDEX TYP TYP 
   INDEX STATUSNIV STATUSNIV TYP
   INDEX TYPCHAR TYPCHAR KALKNR
   INDEX ARTAL ARTAL KALKNR 
   INDEX BENAMNING BENAMNING KALKNR
   INDEX AKTIV AKTIV KALKNR.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_UKALK

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES utvaldfasttemp

/* Definitions for BROWSE BRW_UKALK                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_UKALK utvaldfasttemp.OMRADE ~
utvaldfasttemp.KALKNR utvaldfasttemp.BENAMNING utvaldfasttemp.TYPCHAR ~
utvaldfasttemp.AONR utvaldfasttemp.DELNR utvaldfasttemp.PLANNR ~
utvaldfasttemp.ARTAL utvaldfasttemp.AKTIV 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_UKALK utvaldfasttemp.OMRADE 
&Scoped-define ENABLED-TABLES-IN-QUERY-BRW_UKALK utvaldfasttemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BRW_UKALK utvaldfasttemp
&Scoped-define QUERY-STRING-BRW_UKALK FOR EACH utvaldfasttemp NO-LOCK ~
    BY utvaldfasttemp.OMRADE ~
       BY utvaldfasttemp.AONR ~
        BY utvaldfasttemp.DELNR
&Scoped-define OPEN-QUERY-BRW_UKALK OPEN QUERY BRW_UKALK FOR EACH utvaldfasttemp NO-LOCK ~
    BY utvaldfasttemp.OMRADE ~
       BY utvaldfasttemp.AONR ~
        BY utvaldfasttemp.DELNR.
&Scoped-define TABLES-IN-QUERY-BRW_UKALK utvaldfasttemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_UKALK utvaldfasttemp


/* Definitions for FRAME DEFAULT-FRAME                                  */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_UKALK BTN_AVB 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta":L 
     SIZE 18 BY 1.5.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_UKALK FOR 
      utvaldfasttemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_UKALK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_UKALK C-Win _STRUCTURED
  QUERY BRW_UKALK NO-LOCK DISPLAY
      utvaldfasttemp.OMRADE COLUMN-LABEL "Område" FORMAT "X(6)":U
      utvaldfasttemp.KALKNR COLUMN-LABEL "Kalkyl nr" FORMAT ">>>>>>9":U
            COLUMN-FGCOLOR 1 LABEL-FGCOLOR 1
      utvaldfasttemp.BENAMNING COLUMN-LABEL "Ort/!Benämning" FORMAT "x(20)":U
      utvaldfasttemp.TYPCHAR COLUMN-LABEL "Typ" FORMAT "X(3)":U
      utvaldfasttemp.AONR COLUMN-LABEL "Aonr" FORMAT "X(6)":U COLUMN-FGCOLOR 2
            LABEL-FGCOLOR 2
      utvaldfasttemp.DELNR COLUMN-LABEL "Del!nr" FORMAT ">99":U
            COLUMN-FGCOLOR 2 LABEL-FGCOLOR 2
      utvaldfasttemp.PLANNR COLUMN-LABEL "Plannr" FORMAT "X(6)":U
            COLUMN-FGCOLOR 4 LABEL-FGCOLOR 4
      utvaldfasttemp.ARTAL FORMAT "9999":U COLUMN-FGCOLOR 4 LABEL-FGCOLOR 4
      utvaldfasttemp.AKTIV COLUMN-LABEL "Aktiv/!Inaktiv" FORMAT "Aktiv/Inaktiv":U
  ENABLE
      utvaldfasttemp.OMRADE
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING MULTIPLE SIZE 79 BY 12
         TITLE "Urvalsresultat".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BRW_UKALK AT ROW 10.5 COL 2
     BTN_AVB AT ROW 24.25 COL 32.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80.25 BY 27.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: utvaldfasttemp T "?" NO-UNDO temp-db utvaldfasttemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Kalkylering"
         HEIGHT             = 27.54
         WIDTH              = 83.5
         MAX-HEIGHT         = 28.67
         MAX-WIDTH          = 124.5
         VIRTUAL-HEIGHT     = 28.67
         VIRTUAL-WIDTH      = 124.5
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
/* BROWSE-TAB BRW_UKALK 1 DEFAULT-FRAME */
ASSIGN 
       BRW_UKALK:MAX-DATA-GUESS IN FRAME DEFAULT-FRAME         = 1000.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_UKALK
/* Query rebuild information for BROWSE BRW_UKALK
     _TblList          = "Temp-Tables.utvaldfasttemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.utvaldfasttemp.OMRADE|yes,Temp-Tables.utvaldfasttemp.AONR|yes,Temp-Tables.utvaldfasttemp.DELNR|yes"
     _FldNameList[1]   > Temp-Tables.utvaldfasttemp.OMRADE
"utvaldfasttemp.OMRADE" "Område" ? "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.utvaldfasttemp.KALKNR
"utvaldfasttemp.KALKNR" "Kalkyl nr" ">>>>>>9" "integer" ? 1 ? ? 1 ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > Temp-Tables.utvaldfasttemp.BENAMNING
"utvaldfasttemp.BENAMNING" "Ort/!Benämning" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.utvaldfasttemp.TYPCHAR
"utvaldfasttemp.TYPCHAR" "Typ" "X(3)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.utvaldfasttemp.AONR
"utvaldfasttemp.AONR" "Aonr" ? "character" ? 2 ? ? 2 ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > Temp-Tables.utvaldfasttemp.DELNR
"utvaldfasttemp.DELNR" "Del!nr" ">99" "integer" ? 2 ? ? 2 ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > Temp-Tables.utvaldfasttemp.PLANNR
"utvaldfasttemp.PLANNR" "Plannr" ? "character" ? 4 ? ? 4 ? no ? no no ? yes no no "U" "" ""
     _FldNameList[8]   > Temp-Tables.utvaldfasttemp.ARTAL
"utvaldfasttemp.ARTAL" ? ? "integer" ? 4 ? ? 4 ? no ? no no ? yes no no "U" "" ""
     _FldNameList[9]   > Temp-Tables.utvaldfasttemp.AKTIV
"utvaldfasttemp.AKTIV" "Aktiv/!Inaktiv" "Aktiv/Inaktiv" "logical" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_UKALK */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Kalkylering */
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
ON WINDOW-CLOSE OF C-Win /* Kalkylering */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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


&Scoped-define BROWSE-NAME BRW_UKALK
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
   
   
   RUN disable_UI.
END.
   

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   i = 1.
   REPEAT:
      CREATE utvaldfasttemp.
      utvaldfasttemp.kalknr = i.
      i = i + 1.
      IF i > 100 THEN LEAVE.
   END.
   RUN ENABLE_ui.
   RUN allstartbrw_UI. 
   OPEN QUERY brw_ukalk FOR EACH utvaldfasttemp.
   {&WINDOW-NAME}:HIDDEN = FALSE.
   {&WINDOW-NAME}:MOVE-TO-TOP ().

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
   RUN xDYNBRW.P PERSISTENT SET brwproc[{&LEFT-BROWSE}] 
      (INPUT BRW_UKALK:HANDLE IN FRAME {&FRAME-NAME}).
      RUN setshiftcol_UI IN brwproc[1] (INPUT TRUE).
  
  
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
  ENABLE BRW_UKALK BTN_AVB 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

