&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
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

DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE prostart AS LOGICAL NO-UNDO.
/*DEFINE VARIABLE aostart AS LOGICAL NO-UNDO.*/
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE NEW SHARED TEMP-TABLE smsaonr
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD UTRYCKNING AS LOGICAL
   FIELD PRISTYP AS CHARACTER
   FIELD TRAKTAMENTE AS INTEGER
   FIELD FORETAG AS CHARACTER
   INDEX AONR IS PRIMARY AONR DELNR FORETAG
   INDEX FORE FORETAG AONR DELNR.
   
DEFINE NEW SHARED TEMP-TABLE smspersonal
   FIELD PERSONALKOD AS CHARACTER   
   FIELD ANSTALLNING AS CHARACTER
   FIELD OVERTIDUTTAG AS CHARACTER
   FIELD PREC AS RECID
   FIELD FORETAG AS CHARACTER
   INDEX PERSONALKOD IS PRIMARY PERSONALKOD FORETAG
   INDEX FORE FORETAG PERSONALKOD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-23 aostart BTN_SMS Btn_AONR Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS aostart 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.
DEFINE VARIABLE CtrlFrame-2 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame-2 AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_AONR 
     LABEL "Hämta aonr" 
     SIZE 42 BY 2
     BGCOLOR 8 .

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt / Stoppa" 
     SIZE 42 BY 2
     BGCOLOR 8 .

DEFINE BUTTON BTN_SMS 
     LABEL "Manuell start av sms inläsning" 
     SIZE 42 BY 2.

DEFINE VARIABLE FILL-IN-AOKOLL AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 46 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 47.38 BY 8.38
     BGCOLOR 8 .

DEFINE VARIABLE aostart AS LOGICAL INITIAL no 
     LABEL "Tillfälligt stopp" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.5 BY .79 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FILL-IN-AOKOLL AT ROW 3.46 COL 12 COLON-ALIGNED NO-LABEL
     aostart AT ROW 4.75 COL 14.5
     BTN_SMS AT ROW 6.67 COL 17
     Btn_AONR AT ROW 9.25 COL 17
     Btn_Cancel AT ROW 11.88 COL 17
     "och en gång i timmen läser in aonr från Succel till Guru." VIEW-AS TEXT
          SIZE 59 BY 1 AT ROW 2.08 COL 2.88
     "Detta är ett program som varje min läser in sms uppgifter till Guru" VIEW-AS TEXT
          SIZE 70.13 BY 1 AT ROW 1.17 COL 2.88
     RECT-23 AT ROW 6.13 COL 14.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 72.13 BY 14.75.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Hämtar sms och Aonr från Succel"
         HEIGHT             = 14.75
         WIDTH              = 77.25
         MAX-HEIGHT         = 25.13
         MAX-WIDTH          = 100
         VIRTUAL-HEIGHT     = 25.13
         VIRTUAL-WIDTH      = 100
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
/* SETTINGS FOR FILL-IN FILL-IN-AOKOLL IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       FILL-IN-AOKOLL:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 5.75
       COLUMN          = 6.88
       HEIGHT          = 1.25
       WIDTH           = 3.5
       HIDDEN          = yes
       SENSITIVE       = yes.

CREATE CONTROL-FRAME CtrlFrame-2 ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 8.83
       COLUMN          = 5
       HEIGHT          = 1.25
       WIDTH           = 3.5
       HIDDEN          = yes
       SENSITIVE       = yes.
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      CtrlFrame-2:NAME = "CtrlFrame-2":U .
/* CtrlFrame-2 OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer2 */
      CtrlFrame:MOVE-AFTER(aostart:HANDLE IN FRAME DEFAULT-FRAME).
      CtrlFrame-2:MOVE-AFTER(BTN_SMS:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Hämtar sms och Aonr från Succel */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Hämtar sms och Aonr från Succel */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_AONR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_AONR C-Win
ON CHOOSE OF Btn_AONR IN FRAME DEFAULT-FRAME /* Hämta aonr */
DO:
   C-Win:TITLE = "DEN " + STRING(TODAY) + " KLOCKAN " + 
   STRING(TIME,"HH:MM") + " Startades inläsning av aonr från succel manuellt".    
   aostart = TRUE.
   DISPLAY aostart WITH FRAME {&FRAME-NAME}. 
   RUN hamtaaonr_UI.
   aostart = FALSE.
   DISPLAY aostart WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancel C-Win
ON CHOOSE OF Btn_Cancel IN FRAME DEFAULT-FRAME /* Avbryt / Stoppa */
DO:
   IF CONNECTED("grannord") THEN DO:         
      DISCONNECT grannord NO-ERROR. 
   END.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_SMS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_SMS C-Win
ON CHOOSE OF BTN_SMS IN FRAME DEFAULT-FRAME /* Manuell start av sms inläsning */
DO:
   RUN hamta_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.Tick
PROCEDURE CtrlFrame.PSTimer.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/
   IF TIME >= 68000 AND TIME <= 72000 THEN DO:
      IF CONNECTED("grannord") THEN DO:         
         DISCONNECT grannord NO-ERROR. 
      END.
   END.
   ELSE IF TIME >= 78300 AND TIME <= 86340 THEN DO:
      IF CONNECTED("grannord") THEN DO:         
         DISCONNECT grannord NO-ERROR. 
      END.
   END.   
   ELSE DO:  
      RUN hamta_UI. 
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CtrlFrame-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame-2 C-Win OCX.Tick
PROCEDURE CtrlFrame-2.PSTimer2.Tick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  None required for OCX.
  Notes:       
------------------------------------------------------------------------------*/

   IF TIME >= 68000 AND TIME <= 72000 THEN DO:
      RETURN.
      
   END.
   ELSE IF TIME >= 78300 AND TIME <= 86340 THEN DO:
      RETURN.
   END.   
   ELSE DO:  
       /*
      OUTPUT TO \\GRANGURU\guru_ser\server\PRO9S\tidsao.txt  APPEND.
      PUT "START SUCCAO TIDSTYR " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
      OUTPUT CLOSE.
      C-Win:TITLE = "DEN " + STRING(TODAY) + " KLOCKAN " + 
      STRING(TIME,"HH:MM") + " Startades inläsning av aonr från Succel".   
      aostart = TRUE.
      RUN hamtaaonr_UI.
      aostart = FALSE.
      */
   END.   

END PROCEDURE.

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
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   
   {muswait.i}
   C-Win:TITLE = STRING(TIME,"HH:MM") + " Hämtar sms och Aonr från Succel".   
   prostart = TRUE.  
   {AMERICANEUROPEAN.I}
   OUTPUT TO "c:\temp\niklas.txt" APPEND.
   PUT "1" SKIP.
   OUTPUT CLOSE.
   {EUROPEANAMERICAN.I}
   RUN aopers_UI. 
   ASSIGN
   aostart = FALSE
   prostart = FALSE.

   RUN enable_UI.      
   {musarrow.i}
   {&WINDOW-NAME}:HIDDEN = FALSE.
{&WINDOW-NAME}:MOVE-TO-TOP ().
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aopers_UI C-Win 
PROCEDURE aopers_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   {AMERICANEUROPEAN.I}
   {muswait.i}
   IF CONNECTED("grannord") THEN DO: 
      musz = musz.
   END.
   ELSE DO:   
      OUTPUT TO "c:\temp\niklas.txt" APPEND.
   PUT "2" SKIP.
   OUTPUT CLOSE.
      CONNECT -db GRANNORD -S 2501 -H granguru -N tcp -U "ELPAO" -P "KAGGEN" NO-ERROR.               
      OUTPUT TO "c:\temp\niklas.txt" APPEND.
   PUT "3" SKIP.
   OUTPUT CLOSE.
   END.    
   IF CONNECTED("grannord") THEN DO:
      {VERALIAS.I}
      RUN SMSSKTEM.P.         
      /*DISCONNECT grannord NO-ERROR.*/
   END.    
   OUTPUT TO "c:\temp\niklas.txt" APPEND.
   PUT "9" SKIP.
   OUTPUT CLOSE.
   {EUROPEANAMERICAN.I}
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "SMSSTYR.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
    chCtrlFrame-2 = CtrlFrame-2:COM-HANDLE
    UIB_S = chCtrlFrame-2:LoadControls( OCXFile, "CtrlFrame-2":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "SMSSTYR.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
  RUN control_load.
  DISPLAY aostart 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-23 aostart BTN_SMS Btn_AONR Btn_Cancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hamtaaonr_UI C-Win 
PROCEDURE hamtaaonr_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   RETURN.
   IF OPSYS = "MSDOS" OR OPSYS = "WIN32" THEN DO: 
      OS-COMMAND SILENT \\GRANGURU\guru_ser\server\PRO9S\AO_guru.BAT.
      FILL-IN-AOKOLL = STRING(TIME,"HH:MM") + " " + STRING(TODAY) + " " + "Ftp hämtning klar".
      DISPLAY FILL-IN-AOKOLL WITH FRAME {&FRAME-NAME}. 
      PAUSE 60.
   END.
   filnamn = SEARCH("\\GRANGURU\guru_ser\server\PRO9S\import\aonr_f.s_a").
   IF filnamn = ? THEN DO:
      filnamn = SEARCH("\\GRANGURU\guru_ser\server\PRO9S\import\aonr_f.txt").
   END.
   ELSE DO:
      OS-APPEND \\GRANGURU\guru_ser\server\PRO9S\import\aonr_f.s_a \\GRANGURU\guru_ser\server\PRO9S\import\aonr_f.txt.
      OS-DELETE \\GRANGURU\guru_ser\server\PRO9S\import\aonr_f.s_a.
   END. 
   IF filnamn = ? THEN musz = musz.
   ELSE DO:            
      IF CONNECTED("grannord") THEN DO: 
         musz = musz.
      END.
      ELSE DO:
         CONNECT -db GRANNORD -S 2501 -H granguru -N tcp NO-ERROR.         
      END. 
      IF CONNECTED("grannord") THEN DO:
         {VERALIAS.I}
         C-Win:TITLE = "DEN " + STRING(TODAY) + " KLOCKAN " + 
         STRING(TIME,"HH:MM") + " Startades inläsning av aonr från Succel för Graninge Nord".
         RUN SUCCAO0.P.
         FILL-IN-AOKOLL = STRING(TIME,"HH:MM") + " " + STRING(TODAY) + " " + "Överläsning klar Forse".
         DISPLAY FILL-IN-AOKOLL WITH FRAME {&FRAME-NAME}. 
         DISCONNECT grannord NO-ERROR. 
      END.
   END.   
   filnamn = ?.
   filnamn = SEARCH("\\GRANGURU\guru_ser\server\PRO9S\import\aonr_j.s_a").
   IF filnamn = ? THEN DO:
      filnamn = SEARCH("\\GRANGURU\guru_ser\server\PRO9S\import\aonr_j.txt").
   END.
   ELSE DO:
      OS-APPEND \\GRANGURU\guru_ser\server\PRO9S\import\aonr_j.s_a \\GRANGURU\guru_ser\server\PRO9S\import\aonr_j.txt.
      OS-DELETE \\GRANGURU\guru_ser\server\PRO9S\import\aonr_j.s_a.
   END.
   IF filnamn = ? THEN musz = musz.
   ELSE DO:
      IF CONNECTED("GRANNORD") THEN DO: 
         musz = musz.
      END.
      ELSE DO:
         CONNECT -db GRANNORD -S 2501 -H granguru -N tcp NO-ERROR.         
      END. 
      IF CONNECTED("GRANNORD") THEN DO:
         {VERALIAS.I}
         C-Win:TITLE = "DEN " + STRING(TODAY) + " KLOCKAN " + 
         STRING(TIME,"HH:MM") + " Startades inläsning av aonr från Succel för Järfälla".
         RUN SUCCAO0.P.
         FILL-IN-AOKOLL = STRING(TIME,"HH:MM") + " " + STRING(TODAY) + " " + "Överläsning klar Järfälla".
         DISPLAY FILL-IN-AOKOLL WITH FRAME {&FRAME-NAME}. 
         DISCONNECT GRANNORD NO-ERROR. 
      END.
   END.   
   filnamn = ?.
   filnamn = SEARCH("\\GRANGURU\guru_ser\server\PRO9S\import\aonr_r.s_a").
   IF filnamn = ? THEN DO:
      filnamn = SEARCH("\\GRANGURU\guru_ser\server\PRO9S\import\aonr_r.txt").
   END.
   ELSE DO:
      OS-APPEND \\GRANGURU\guru_ser\server\PRO9S\import\aonr_r.s_a \\GRANGURU\guru_ser\server\PRO9S\import\aonr_r.txt.
      OS-DELETE \\GRANGURU\guru_ser\server\PRO9S\import\aonr_r.s_a.
   END.
   IF filnamn = ? THEN musz = musz.
   ELSE DO:
      IF CONNECTED("GRANNORD") THEN DO: 
         musz = musz.
      END.
      ELSE DO:
         CONNECT -db GRANNORD -S 2510 -H granguru -N tcp NO-ERROR.         
      END. 
      IF CONNECTED("GRANNORD") THEN DO:
         {VERALIAS.I}
         C-Win:TITLE = "DEN " + STRING(TODAY) + " KLOCKAN " + 
         STRING(TIME,"HH:MM") + " Startades inläsning av aonr från Succel för Roslagen".
         RUN SUCCAO0.P.
         FILL-IN-AOKOLL = STRING(TIME,"HH:MM") + " " + STRING(TODAY) + " " + "Överläsning klar Roslagen".
         DISPLAY FILL-IN-AOKOLL WITH FRAME {&FRAME-NAME}. 
         DISCONNECT GRANNORD NO-ERROR. 
      END.
   END.
   filnamn = ?.
   filnamn = SEARCH("\\GRANGURU\guru_ser\server\PRO9S\import\aonr_m.s_a").
   IF filnamn = ? THEN DO:
      filnamn = SEARCH("\\GRANGURU\guru_ser\server\PRO9S\import\aonr_m.txt").
   END.
   ELSE DO:
      OS-APPEND \\GRANGURU\guru_ser\server\PRO9S\import\aonr_m.s_a \\GRANGURU\guru_ser\server\PRO9S\import\aonr_m.txt.
      OS-DELETE \\GRANGURU\guru_ser\server\PRO9S\import\aonr_m.s_a.
   END.
   IF filnamn = ? THEN musz = musz.
   ELSE DO:
      IF CONNECTED("GRANNORD") THEN DO: 
         musz = musz.
      END.
      ELSE DO:
         CONNECT -db GRANNORD -S 2501 -H granguru -N tcp NO-ERROR.         
      END. 
      IF CONNECTED("GRANNORD") THEN DO:
         {VERALIAS.I}
         C-Win:TITLE = "DEN " + STRING(TODAY) + " KLOCKAN " + 
         STRING(TIME,"HH:MM") + " Startades inläsning av aonr från Succel för Mälar.".
         RUN SUCCAO0.P.
         FILL-IN-AOKOLL = STRING(TIME,"HH:MM") + " " + STRING(TODAY) + " " + "Överläsning klar Mälar.".
         DISPLAY FILL-IN-AOKOLL WITH FRAME {&FRAME-NAME}. 
         DISCONNECT GRANNORD NO-ERROR. 
      END.
   END.   
   filnamn = ?.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hamta_UI C-Win 
PROCEDURE hamta_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   IF aostart = TRUE THEN RETURN.
   {muswait.i}
   C-Win:TITLE = "DEN " + STRING(TODAY) + " KLOCKAN " + 
   STRING(TIME,"HH:MM") + " STARTADES INLÄSNING AV SMS-MEDDELANDE".     
   IF prostart = FALSE THEN DO:
      prostart = TRUE.
      IF TIME >= 21600 AND TIME <= 21720 THEN DO:
         RUN aopers_UI.
      END.
      ELSE IF TIME >= 42320 AND TIME <= 43200 THEN DO:
         RUN aopers_UI.
      END. 
      FIND FIRST smspersonal NO-LOCK NO-ERROR.
      IF AVAILABLE smspersonal THEN RUN SMSDB.P. 
      ELSE RUN aopers_UI.
      prostart = FALSE.
   END.   
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

