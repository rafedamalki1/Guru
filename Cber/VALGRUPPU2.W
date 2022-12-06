&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME WINDOW-1



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-1 
/*------------------------------------------------------------------------

  File: 

  Description: f

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
&Scoped-define NEW 
{GLOBVAR2DEL1.I}

{HOPALLA.I}
&Scoped-define SHARED SHARED
{KONSTRMTRL.I}
{BBENAMNTEMP.I}
DEFINE NEW SHARED VARIABLE varalla AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE valgrupp AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE lista AS INTEGER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE kgrprak AS INTEGER NO-UNDO.
DEFINE VARIABLE grprak AS INTEGER NO-UNDO.

DEFINE NEW SHARED TEMP-TABLE grupp_temp NO-UNDO
   FIELD KONSKOD AS INTEGER
   FIELD BENAMNING AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define BROWSE-NAME BRW_GRUPP

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES konstgrptemp

/* Definitions for BROWSE BRW_GRUPP                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_GRUPP konstgrptemp.BENAMNING 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_GRUPP 
&Scoped-define QUERY-STRING-BRW_GRUPP FOR EACH konstgrptemp NO-LOCK ~
    BY konstgrptemp.BENAMNING
&Scoped-define OPEN-QUERY-BRW_GRUPP OPEN QUERY BRW_GRUPP FOR EACH konstgrptemp NO-LOCK ~
    BY konstgrptemp.BENAMNING.
&Scoped-define TABLES-IN-QUERY-BRW_GRUPP konstgrptemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_GRUPP konstgrptemp


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS TOG_ALLA BRW_GRUPP BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS TOG_ALLA 

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

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE RAD_SCH AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Ej schakt", 1,
"Enbart schakt", 2,
"Plus schakt", 3
     SIZE 44.5 BY 1 NO-UNDO.

DEFINE VARIABLE TOG_ALLA AS LOGICAL INITIAL no 
     LABEL "Alla" 
     VIEW-AS TOGGLE-BOX
     SIZE 7.75 BY .88 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_GRUPP FOR 
      konstgrptemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_GRUPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_GRUPP WINDOW-1 _STRUCTURED
  QUERY BRW_GRUPP NO-LOCK DISPLAY
      konstgrptemp.BENAMNING FORMAT "X(256)":U WIDTH 45
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-LABELS NO-COLUMN-SCROLLING MULTIPLE SIZE 56 BY 25
         TITLE "Välj konstruktionsgrupp".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     RAD_SCH AT ROW 1.25 COL 12.5 NO-LABEL
     TOG_ALLA AT ROW 1.38 COL 2.75
     BRW_GRUPP AT ROW 2.5 COL 1.5
     BTN_OK AT ROW 28.04 COL 28.63
     BTN_AVB AT ROW 28.04 COL 43.63
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 57.75 BY 28.42.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
   Temp-Tables and Buffers:
      TABLE: konstgrptemp T "?" NO-UNDO temp-db konstgrptemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = ""
         HEIGHT             = 28.42
         WIDTH              = 57.88
         MAX-HEIGHT         = 37.67
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 37.67
         VIRTUAL-WIDTH      = 160
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
/* BROWSE-TAB BRW_GRUPP TOG_ALLA FRAME-A */
ASSIGN 
       BRW_GRUPP:MAX-DATA-GUESS IN FRAME FRAME-A         = 1000.

/* SETTINGS FOR RADIO-SET RAD_SCH IN FRAME FRAME-A
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       RAD_SCH:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_GRUPP
/* Query rebuild information for BROWSE BRW_GRUPP
     _TblList          = "Temp-Tables.konstgrptemp"
     _Options          = "NO-LOCK"
     _OrdList          = "Temp-Tables.konstgrptemp.BENAMNING|yes"
     _FldNameList[1]   > Temp-Tables.konstgrptemp.BENAMNING
"konstgrptemp.BENAMNING" ? "X(256)" "character" ? ? ? ? ? ? no ? no no "45" yes no no "U" "" ""
     _Query            is NOT OPENED
*/  /* BROWSE BRW_GRUPP */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avbryt */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK WINDOW-1
ON CHOOSE OF BTN_OK IN FRAME FRAME-A /* Ok */
DO:
   {muswait.i}   
   antal_valda = BRW_GRUPP:NUM-SELECTED-ROWS.
   antal_raknare = 1.
   IF RAD_SCH = 2 THEN.
   ELSE DO:   
      IF antal_valda = 0 THEN DO:
         MESSAGE "Ni måste välja någon konstruktionsgrupp." VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
   END.
   
   EMPTY TEMP-TABLE grupp_temp NO-ERROR.
   DO WHILE antal_raknare LE antal_valda:                                   
      status-ok = BRW_GRUPP:FETCH-SELECTED-ROW(antal_raknare).
      IF AVAILABLE konstgrptemp THEN DO:
         CREATE grupp_temp.
         ASSIGN
         grupp_temp.KONSKOD = konstgrptemp.KONSKOD
         grupp_temp.BENAMNING = konstgrptemp.BENAMNING
         valgrupp = konstgrptemp.KONSKOD. 
      END.
      antal_raknare = antal_raknare + 1.   
   END.
   
   
   {AVBGOM.I}
  
   grprak = 0.
   kgrprak = 0.
   FOR EACH grupp_temp WHERE NO-LOCK:
      grprak = grprak + 1.
   END.
   FOR EACH konstgrptemp WHERE NO-LOCK:
      kgrprak = kgrprak + 1.
   END.
   varalla = FALSE.
   IF grprak = kgrprak THEN DO:
      /*hela upplägget = även BERSTOLP ska med*/
      varalla = TRUE.
   END.
   IF lista = 1 THEN RUN MTXTILLY.W.
   ELSE IF lista = 2 THEN RUN KTXTILLY.W (INPUT RAD_SCH).
   ELSE IF lista = 3 THEN RUN VISUNGRP.W .
   {AVBFRAM.I}  
   {musarrow.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_SCH
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_SCH WINDOW-1
ON VALUE-CHANGED OF RAD_SCH IN FRAME FRAME-A
DO:
  RAD_SCH = INPUT RAD_SCH.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_ALLA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_ALLA WINDOW-1
ON VALUE-CHANGED OF TOG_ALLA IN FRAME FRAME-A /* Alla */
DO:
   TOG_ALLA = INPUT TOG_ALLA.
   
   IF TOG_ALLA = TRUE THEN APPLY "CTRL-A" TO BRW_GRUPP.
   ELSE status-ok = BRW_GRUPP:DESELECT-ROWS() IN FRAME {&FRAME-NAME} NO-ERROR.
   /*
   
   IF TOG_ALLA = TRUE THEN DO:
      BRW_GRUPP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
   END.
   ELSE DO:
      BRW_GRUPP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE.
   END.
    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_GRUPP
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
   ASSIGN
   TOG_ALLA = FALSE
   varalla = FALSE.
   RUN enable_UI.   
   {FRMSIZE.I} 
   IF lista = 2 THEN DO:
      RAD_SCH = 1.        
      IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "FORS" OR Guru.Konstanter:beresekvar[13]  = TRUE THEN DO:       
         ENABLE RAD_SCH WITH FRAME {&FRAME-NAME}.
         DISPLAY RAD_SCH WITH FRAME {&FRAME-NAME}.
      END.
   END.
   RUN setcolsortvar_UI IN brwproc[1] (INPUT "").
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
  Purpose:      
  Parameters:  
  Notes:       
-------------------------------------------------------------*/ 
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_GRUPP:HANDLE IN FRAME {&FRAME-NAME}).  
   RUN setcolindex_UI IN brwproc[1] (INPUT "ORDNING").
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
  DISPLAY TOG_ALLA 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE TOG_ALLA BRW_GRUPP BTN_OK BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

