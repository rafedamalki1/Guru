&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME WINDOW-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-2 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 12/12/96 -  9:29 am

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

&Scoped-define SHARED SHARED 
{KONVALTEMP.I}
DEFINE NEW SHARED VARIABLE lev1 AS CHARACTER NO-UNDO.    
DEFINE NEW SHARED VARIABLE lev2 AS CHARACTER NO-UNDO.  
DEFINE NEW SHARED VARIABLE lev3 AS CHARACTER NO-UNDO.  
DEFINE NEW SHARED VARIABLE RAD_STOLP AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE stolpval AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE valkonst AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE vald_kundlev AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valort AS CHARACTER NO-UNDO. 
DEFINE SHARED VARIABLE valbernr AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE VARIABLE vlev AS CHARACTER NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE bytlev3apph AS HANDLE NO-UNDO.

{JMFLEVTEP.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME FRAME-B

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CMB_LEV1 CMB_LEV2 CMB_LEV3 BTN_VISALIST ~
RAD_OPT TOG_KONVAL TOG_VEJT TOG_VHU BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS CMB_LEV1 CMB_LEV2 CMB_LEV3 RAD_OPT ~
TOG_KONVAL TOG_VEJT TOG_VHU 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-2 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VISALIST 
     LABEL "Visa" 
     SIZE 14 BY 1.

DEFINE VARIABLE CMB_LEV1 AS CHARACTER FORMAT "X(15)":U 
     LABEL "Leverantör 1" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18.25 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_LEV2 AS CHARACTER FORMAT "X(15)":U 
     LABEL "Leverantör 2" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18.25 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_LEV3 AS CHARACTER FORMAT "X(15)":U 
     LABEL "Leverantör 3" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 18.25 BY 1 NO-UNDO.

DEFINE VARIABLE RAD_OPT AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Optimera per beredning", 1,
"Optimera per konstruktion", 2,
"Optimera per artikel", 3
     SIZE 75 BY 1 NO-UNDO.

DEFINE VARIABLE TOG_KONVAL AS LOGICAL INITIAL no 
     LABEL "Vill Ni välja konstruktioner?" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_VEJT AS LOGICAL INITIAL no 
     LABEL "Visa icke-träffar i bilaga i excel" 
     VIEW-AS TOGGLE-BOX
     SIZE 32.5 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_VHU AS LOGICAL INITIAL no 
     LABEL "Vid icke-träff ta ej med enr för någon leverantör vid summering" 
     VIEW-AS TOGGLE-BOX
     SIZE 68 BY .79 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-B
     CMB_LEV1 AT ROW 2.5 COL 14 COLON-ALIGNED
     CMB_LEV2 AT ROW 2.5 COL 46.5 COLON-ALIGNED
     CMB_LEV3 AT ROW 2.5 COL 80 COLON-ALIGNED
     BTN_VISALIST AT ROW 2.75 COL 103
     RAD_OPT AT ROW 4 COL 15.88 NO-LABEL
     TOG_KONVAL AT ROW 5.75 COL 15.5
     TOG_VEJT AT ROW 6.88 COL 15.5
     TOG_VHU AT ROW 8 COL 15.5
     BTN_AVB AT ROW 10.58 COL 103
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 116.75 BY 11.04.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-2 ASSIGN
         HIDDEN             = YES
         TITLE              = "Jämför priser"
         HEIGHT             = 11.04
         WIDTH              = 116.75
         MAX-HEIGHT         = 22
         MAX-WIDTH          = 116.75
         VIRTUAL-HEIGHT     = 22
         VIRTUAL-WIDTH      = 116.75
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
/* SETTINGS FOR WINDOW WINDOW-2
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-B
   FRAME-NAME                                                           */
ASSIGN 
       CMB_LEV1:HIDDEN IN FRAME FRAME-B           = TRUE.

ASSIGN 
       CMB_LEV2:HIDDEN IN FRAME FRAME-B           = TRUE.

ASSIGN 
       CMB_LEV3:HIDDEN IN FRAME FRAME-B           = TRUE.

ASSIGN 
       TOG_KONVAL:HIDDEN IN FRAME FRAME-B           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
THEN WINDOW-2:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-2
ON CHOOSE OF BTN_AVB IN FRAME FRAME-B /* Avbryt */
DO:       
   APPLY "CLOSE":U TO THIS-PROCEDURE.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_VISALIST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VISALIST WINDOW-2
ON CHOOSE OF BTN_VISALIST IN FRAME FRAME-B /* Visa */
DO:
  RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEV1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEV1 WINDOW-2
ON VALUE-CHANGED OF CMB_LEV1 IN FRAME FRAME-B /* Leverantör 1 */
DO:                                 
   CMB_LEV1 = INPUT CMB_LEV1. 
   FIND FIRST jmflev_temp WHERE jmflev_temp.LEVNAMN = CMB_LEV1 USE-INDEX LEVKOD NO-LOCK NO-ERROR. 
   IF AVAILABLE jmflev_temp THEN lev1 = jmflev_temp.LEVKOD.
   ELSE lev1 = "".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEV2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEV2 WINDOW-2
ON VALUE-CHANGED OF CMB_LEV2 IN FRAME FRAME-B /* Leverantör 2 */
DO:                                 
   CMB_LEV2 = INPUT CMB_LEV2.  
   FIND FIRST jmflev_temp WHERE jmflev_temp.LEVNAMN = CMB_LEV2 USE-INDEX LEVKOD NO-LOCK NO-ERROR.    
   IF AVAILABLE jmflev_temp THEN lev2 = jmflev_temp.LEVKOD.
   ELSE lev2 = "".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEV3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEV3 WINDOW-2
ON VALUE-CHANGED OF CMB_LEV3 IN FRAME FRAME-B /* Leverantör 3 */
DO:                                 
   CMB_LEV3 = INPUT CMB_LEV3.  
   FIND FIRST jmflev_temp WHERE jmflev_temp.LEVNAMN = CMB_LEV3 USE-INDEX LEVKOD NO-LOCK NO-ERROR. 
   IF AVAILABLE jmflev_temp THEN lev3 = jmflev_temp.LEVKOD.
   ELSE lev3 = "".   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_OPT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_OPT WINDOW-2
ON VALUE-CHANGED OF RAD_OPT IN FRAME FRAME-B
DO:
  RAD_OPT = INPUT RAD_OPT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_KONVAL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_KONVAL WINDOW-2
ON VALUE-CHANGED OF TOG_KONVAL IN FRAME FRAME-B /* Vill Ni välja konstruktioner? */
DO:
  TOG_KONVAL = INPUT TOG_KONVAL.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_VEJT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_VEJT WINDOW-2
ON VALUE-CHANGED OF TOG_VEJT IN FRAME FRAME-B /* Visa icke-träffar i bilaga i excel */
DO:
  TOG_VEJT = INPUT TOG_VEJT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_VHU
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_VHU WINDOW-2
ON VALUE-CHANGED OF TOG_VHU IN FRAME FRAME-B /* Vid icke-träff ta ej med enr för någon leverantör vid summering */
DO:
  TOG_VHU = INPUT TOG_VHU.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-2 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
    IF VALID-HANDLE(bytlev3apph) THEN DELETE PROCEDURE bytlev3apph.
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
   {&WINDOW-NAME}:TITLE = "Välj leverantörer för prisjämförelse i beredning " + valaonr + " " + valort.         
    EMPTY TEMP-TABLE jmflev_temp NO-ERROR. 
    RUN skapalista_UI IN bytlev3apph (OUTPUT TABLE jmflev_temp, OUTPUT vald_kundlev).     
    /*test lena att hämta beredningens leverantör*/
    RUN hamtlev_UI IN bytlev3apph (INPUT valomrade,INPUT valbernr,OUTPUT vlev).        
    IF vlev = "" THEN DO:
       vlev = vald_kundlev.
    END.
    
   
   ASSIGN         
   status-ok = CMB_LEV1:DELETE("0")
   status-ok = CMB_LEV2:DELETE("0"). 
   status-ok = CMB_LEV3:DELETE("0"). 
   /*  lena 
   FIND FIRST jmflev_temp WHERE jmflev_temp.LEVKOD = vald_kundlev USE-INDEX LEVKOD NO-LOCK NO-ERROR. */
   IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
      FOR EACH jmflev_temp USE-INDEX LEVKOD NO-LOCK:
         IF jmflev_temp.LEVKOD = "1" OR jmflev_temp.LEVKOD = "2" OR jmflev_temp.LEVKOD = "8" OR jmflev_temp.LEVKOD = "5" THEN DO:
            status-ok = CMB_LEV1:ADD-LAST(jmflev_temp.LEVNAMN).
            status-ok = CMB_LEV2:ADD-LAST(jmflev_temp.LEVNAMN).
            status-ok = CMB_LEV3:ADD-LAST(jmflev_temp.LEVNAMN).
         END.
      END.
      IF Guru.Konstanter:appcon THEN DO:
         RUN  MinneHmt.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globanv + "$" + "INK1", OUTPUT CMB_LEV1).
         RUN  MinneHmt.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globanv + "$" + "INK2", OUTPUT CMB_LEV2).
         RUN  MinneHmt.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globanv + "$" + "INK3", OUTPUT CMB_LEV3). 
      END.
      ELSE DO:
         RUN  MinneHmt.P (INPUT Guru.Konstanter:globanv + "$" + "INK1", OUTPUT CMB_LEV1).
         RUN  MinneHmt.P (INPUT Guru.Konstanter:globanv + "$" + "INK2", OUTPUT CMB_LEV2).
         RUN  MinneHmt.P (INPUT Guru.Konstanter:globanv + "$" + "INK3", OUTPUT CMB_LEV3).
      END.  
      
      IF CMB_LEV1 NE "" THEN CMB_LEV1:SCREEN-VALUE = CMB_LEV1.
      IF CMB_LEV2 NE "" THEN CMB_LEV2:SCREEN-VALUE = CMB_LEV2.
      IF CMB_LEV3 NE "" THEN CMB_LEV3:SCREEN-VALUE = CMB_LEV3.
            
      FIND FIRST jmflev_temp WHERE jmflev_temp.LEVNAMN = CMB_LEV1 USE-INDEX LEVKOD NO-LOCK NO-ERROR.  
      lev1 = jmflev_temp.LEVKOD.
      FIND FIRST jmflev_temp WHERE jmflev_temp.LEVNAMN = CMB_LEV2 USE-INDEX LEVKOD NO-LOCK NO-ERROR.  
      lev2 = jmflev_temp.LEVKOD.
      FIND FIRST jmflev_temp WHERE jmflev_temp.LEVNAMN = CMB_LEV3 USE-INDEX LEVKOD NO-LOCK NO-ERROR.  
      lev3 = jmflev_temp.LEVKOD.
            
   END.
   ELSE DO:
      FIND FIRST jmflev_temp WHERE jmflev_temp.LEVKOD = vlev USE-INDEX LEVKOD NO-LOCK NO-ERROR. 
      ASSIGN         
      status-ok = CMB_LEV1:ADD-LAST(jmflev_temp.LEVNAMN)IN FRAME {&FRAME-NAME}     
      CMB_LEV1:SCREEN-VALUE = jmflev_temp.LEVNAMN. 
      lev1 = jmflev_temp.LEVKOD.
      
      FOR EACH jmflev_temp WHERE jmflev_temp.LEVKOD NE vlev USE-INDEX LEVKOD NO-LOCK:
         ASSIGN      
         status-ok = CMB_LEV1:ADD-LAST(jmflev_temp.LEVNAMN)IN FRAME {&FRAME-NAME}.      
      END.        
      ASSIGN
      status-ok = CMB_LEV2:ADD-LAST("")IN FRAME {&FRAME-NAME}
      status-ok = CMB_LEV3:ADD-LAST("")IN FRAME {&FRAME-NAME}
      CMB_LEV2:SCREEN-VALUE = ""   
      CMB_LEV3:SCREEN-VALUE = "".
      FOR EACH jmflev_temp USE-INDEX LEVKOD NO-LOCK:
         ASSIGN          
         status-ok = CMB_LEV2:ADD-LAST(jmflev_temp.LEVNAMN)IN FRAME {&FRAME-NAME}.              
         status-ok = CMB_LEV3:ADD-LAST(jmflev_temp.LEVNAMN)IN FRAME {&FRAME-NAME}.              
      END.        
      ASSIGN     
      CMB_LEV1 = INPUT CMB_LEV1
      CMB_LEV2 = INPUT CMB_LEV2
      CMB_LEV3 = INPUT CMB_LEV3.   
   END.
   RAD_OPT = 1.
   TOG_VHU = TRUE.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI WINDOW-2 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose:    
  Parameters: 
  Notes:       
-------------------------------------------------------------*/    
   IF Guru.Konstanter:appcon THEN DO:
      RUN BYTLEV3APP.P PERSISTENT SET bytlev3apph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN BYTLEV3APP.P PERSISTENT SET bytlev3apph.
   END. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-2  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-2)
  THEN DELETE WIDGET WINDOW-2.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-2  _DEFAULT-ENABLE
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
  DISPLAY CMB_LEV1 CMB_LEV2 CMB_LEV3 RAD_OPT TOG_KONVAL TOG_VEJT TOG_VHU 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  ENABLE CMB_LEV1 CMB_LEV2 CMB_LEV3 BTN_VISALIST RAD_OPT TOG_KONVAL TOG_VEJT 
         TOG_VHU BTN_AVB 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sparakon_UI WINDOW-2 
PROCEDURE sparakon_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/   
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.   
   IF vad = 1 THEN DO:
      EMPTY TEMP-TABLE sparakon_val NO-ERROR. 
      FOR EACH kon_val:
         CREATE sparakon_val.
         BUFFER-COPY kon_val TO sparakon_val.         
      END.
      EMPTY TEMP-TABLE kon_val NO-ERROR. 
   END.
   IF vad = 2 THEN DO:
      EMPTY TEMP-TABLE kon_val NO-ERROR.
      FOR EACH sparakon_val:
         CREATE kon_val.
         BUFFER-COPY sparakon_val TO kon_val.         
      END.
      EMPTY TEMP-TABLE sparakon_val NO-ERROR. 
      /*IF VALID-HANDLE(btnbervalh) THEN RUN listhelp_UI IN btnbervalh.*/
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visa_UI WINDOW-2 
PROCEDURE visa_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {muswait.i}   
   DEFINE VARIABLE utfil AS CHARACTER NO-UNDO.   
   {AVBGOM.I}  
   IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
      IF Guru.Konstanter:appcon THEN DO:
         RUN  MinneSpar.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globanv + "$" + "INK1", INPUT CMB_LEV1).
         RUN  MinneSpar.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globanv + "$" + "INK2", INPUT CMB_LEV2).
         RUN  MinneSpar.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globanv + "$" + "INK3", INPUT CMB_LEV3). 
      END.
      ELSE DO:
         RUN  MinneSpar.P (INPUT Guru.Konstanter:globanv + "$" + "INK1", INPUT CMB_LEV1).
         RUN  MinneSpar.P (INPUT Guru.Konstanter:globanv + "$" + "INK2", INPUT CMB_LEV2).
         RUN  MinneSpar.P (INPUT Guru.Konstanter:globanv + "$" + "INK3", INPUT CMB_LEV3).
      END.  
   END.
   
   musz = FALSE.
   IF TOG_KONVAL:CHECKED IN FRAME {&FRAME-NAME} = TRUE THEN DO:
      RUN sparakon_UI (INPUT 1).
      valkonst = TRUE.
      RUN VALKONSTU.W.
   END.
   ELSE DO:
      valkonst = FALSE.
   END.      
   IF musz = FALSE THEN DO:         
      RUN BERLISTJAM.W (INPUT lev1,INPUT lev2,INPUT lev3,INPUT TABLE jmflev_temp,INPUT RAD_OPT,INPUT TOG_VEJT,INPUT TOG_VHU).   
   END.      
   IF TOG_KONVAL:CHECKED = TRUE THEN DO:
      RUN sparakon_UI (INPUT 2).
   END.

   musz = FALSE.
   {AVBFRAM.I}
   {musarrow.i}  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

