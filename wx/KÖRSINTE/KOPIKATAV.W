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

  Created: 10/15/96 -  9:58 am

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
DEFINE SHARED VARIABLE leverant AS CHARACTER NO-UNDO.
DEFINE VARIABLE leverant2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE curr_lev AS RECID NO-UNDO.
DEFINE VARIABLE OKvald AS LOGICAL INITIAL TRUE NO-UNDO.
DEFINE VARIABLE mtrlbapph AS HANDLE NO-UNDO.
&Scoped-define NEW
&Scoped-define SHARED SHARED
{LEVTEMP.I}

DEFINE TEMP-TABLE lev_temp2
   FIELD LEVNAMN AS CHARACTER LABEL "Leverantör"
   FIELD LEVKOD AS CHARACTER.

/*DEFINE BUFFER mtrlbuff FOR MTRL.*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_LEV

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES lev_temp2

/* Definitions for BROWSE BRW_LEV                                       */
&Scoped-define FIELDS-IN-QUERY-BRW_LEV lev_temp2.LEVKOD lev_temp2.LEVNAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_LEV 
&Scoped-define QUERY-STRING-BRW_LEV FOR EACH lev_temp2 NO-LOCK ~
    BY lev_temp2.LEVKOD
&Scoped-define OPEN-QUERY-BRW_LEV OPEN QUERY BRW_LEV FOR EACH lev_temp2 NO-LOCK ~
    BY lev_temp2.LEVKOD.
&Scoped-define TABLES-IN-QUERY-BRW_LEV lev_temp2
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_LEV lev_temp2


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_LEV BTN_OK BTN_AVSL 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVSL AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_LEV FOR 
      lev_temp2 SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_LEV WINDOW-1 _STRUCTURED
  QUERY BRW_LEV NO-LOCK DISPLAY
      lev_temp2.LEVKOD COLUMN-LABEL "Lev-id" FORMAT "x(4)":U
      lev_temp2.LEVNAMN FORMAT "x(256)":U WIDTH 29.5
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SIZE 39 BY 17.13
         TITLE "Leverantörer".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BRW_LEV AT ROW 1.5 COL 1.5
     BTN_OK AT ROW 19 COL 11.5
     BTN_AVSL AT ROW 19 COL 26.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 40.63 BY 19.33.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: 
   Temp-Tables and Buffers:
      TABLE: ? T "?" NO-UNDO temp-db lev_temp2
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Välj leverantör som skall kopieras in i katalog"
         HEIGHT             = 19.33
         WIDTH              = 40.63
         MAX-HEIGHT         = 22.88
         MAX-WIDTH          = 82
         VIRTUAL-HEIGHT     = 22.88
         VIRTUAL-WIDTH      = 82
         RESIZE             = yes
         SCROLL-BARS        = yes
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
/* SETTINGS FOR WINDOW WINDOW-1
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
/* BROWSE-TAB BRW_LEV 1 FRAME-A */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_LEV
/* Query rebuild information for BROWSE BRW_LEV
     _TblList          = "Temp-Tables.lev_temp2"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.lev_temp2.LEVKOD|yes"
     _FldNameList[1]   > Temp-Tables.lev_temp2.LEVKOD
"lev_temp2.LEVKOD" "Lev-id" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.lev_temp2.LEVNAMN
"lev_temp2.LEVNAMN" ? "x(256)" "character" ? ? ? ? ? ? no ? no no "29.5" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_LEV */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define BROWSE-NAME BRW_LEV
&Scoped-define SELF-NAME BRW_LEV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_LEV WINDOW-1
ON VALUE-CHANGED OF BRW_LEV IN FRAME FRAME-A /* Leverantörer */
DO: 
   curr_lev = RECID(lev_temp2).
   leverant2 = lev_temp2.LEVKOD.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVSL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVSL WINDOW-1
ON CHOOSE OF BTN_AVSL IN FRAME FRAME-A /* Avbryt */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK WINDOW-1
ON CHOOSE OF BTN_OK IN FRAME FRAME-A /* Ok */
DO:
   FIND FIRST levtemp WHERE levtemp.LEVKOD = lev_temp2.LEVKOD NO-LOCK NO-ERROR.
   MESSAGE "Vill du kopiera in katalogen för leverantör:" + levtemp.LEVNAMN
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO UPDATE svar AS LOGICAL.         
   IF svar THEN DO:                
      RUN kopikat2_UI IN mtrlbapph (INPUT  lev_temp2.LEVKOD, INPUT leverant). 
      MESSAGE "Katalogen är nu inkopierad." VIEW-AS ALERT-BOX.
      /*OPEN QUERY mtrlq FOR EACH MTRL WHERE MTRL.LEVKOD = lev_temp2.LEVKOD AND
      MTRL.KALKNR = 0 USE-INDEX LEV NO-LOCK.
      GET FIRST mtrlq NO-LOCK.
      DO WHILE AVAILABLE(MTRL):
         FIND FIRST mtrlbuff WHERE mtrlbuff.ENR = MTRL.ENR AND mtrlbuff.LEVKOD = leverant
         AND mtrlbuff.KALKNR = 0 NO-LOCK NO-ERROR.
         IF AVAILABLE mtrlbuff THEN leverant = leverant.
         ELSE DO TRANSACTION:
            CREATE mtrlbuff.
            ASSIGN
            mtrlbuff.ENR = MTRL.ENR
            mtrlbuff.BENAMNING = MTRL.BENAMNING
            mtrlbuff.ENHET = MTRL.ENHET
            mtrlbuff.LEVKOD = leverant
            mtrlbuff.NPRIS = MTRL.NPRIS
            mtrlbuff.BPRIS = MTRL.BPRIS
            mtrlbuff.KALKNR = 0.
         END.                  
         GET NEXT mtrlq NO-LOCK.
      END.            */
   END.
   ELSE DO:
      RETURN NO-APPLY.
   END.   
   APPLY "CLOSE":U TO THIS-PROCEDURE.
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
   FOR EACH levtemp WHERE levtemp.LEVKOD NE "0" AND levtemp.BORTTAG = FALSE NO-LOCK:
      IF levtemp.LEVKOD NE leverant THEN DO:
         CREATE lev_temp2.
         ASSIGN
         lev_temp2.LEVKOD = levtemp.LEVKOD
         lev_temp2.LEVNAMN = levtemp.LEVNAMN.
      END.   
   END.   
   OPEN QUERY BRW_LEV FOR EACH lev_temp2 NO-LOCK BY lev_temp2.LEVKOD.
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
      (INPUT BRW_LEV:HANDLE IN FRAME {&FRAME-NAME}).
   
   IF Guru.Konstanter:appcon THEN DO:
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN MTRLBAPP.P PERSISTENT SET mtrlbapph.
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
  ENABLE BRW_LEV BTN_OK BTN_AVSL 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

