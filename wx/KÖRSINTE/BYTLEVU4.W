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
{LEVTEMP.I}
{HUVLEVTEMP.I}
{KONSTRMTRL.I}

DEFINE NEW SHARED VARIABLE lev1 AS CHARACTER NO-UNDO.    
DEFINE NEW SHARED VARIABLE lev2 AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE huvudlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vald_kundlev AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE vald_lev AS CHARACTER NO-UNDO.  
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE bytlevapph AS HANDLE NO-UNDO.
DEFINE VARIABLE val AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-B

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CMB_LEVFR CMB_LEVTILL BTN_BEST BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS CMB_LEVFR CMB_LEVTILL 

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

DEFINE BUTTON BTN_BEST 
     LABEL "Ok" 
     SIZE 14 BY 1
     FGCOLOR 1 .

DEFINE VARIABLE CMB_LEVFR AS CHARACTER FORMAT "X(15)":U 
     LABEL "Från leverantör" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 19.75 BY 1 NO-UNDO.

DEFINE VARIABLE CMB_LEVTILL AS CHARACTER FORMAT "X(15)":U 
     LABEL "Till leverantör" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 19.75 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-B
     CMB_LEVFR AT ROW 2.38 COL 19.25 COLON-ALIGNED
     CMB_LEVTILL AT ROW 5.04 COL 19.25 COLON-ALIGNED
     BTN_BEST AT ROW 7 COL 11.75
     BTN_AVB AT ROW 7 COL 26.75
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 41.13 BY 7.46.


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
         TITLE              = "Byt leverantör"
         HEIGHT             = 7.46
         WIDTH              = 41.13
         MAX-HEIGHT         = 22
         MAX-WIDTH          = 82
         VIRTUAL-HEIGHT     = 22
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
/* SETTINGS FOR WINDOW WINDOW-2
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-B
                                                                        */
ASSIGN 
       CMB_LEVFR:HIDDEN IN FRAME FRAME-B           = TRUE.

ASSIGN 
       CMB_LEVTILL:HIDDEN IN FRAME FRAME-B           = TRUE.

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


&Scoped-define SELF-NAME BTN_BEST
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BEST WINDOW-2
ON CHOOSE OF BTN_BEST IN FRAME FRAME-B /* Ok */
DO:  
   IF lev1 = lev2 THEN DO:
      MESSAGE "Det går ej att utföra byte till samma leverantör." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   ELSE DO: 
      ASSIGN
      felmedd = ""
      val = 0.
      RUN kontroll_UI IN bytlevapph (INPUT lev1,INPUT lev2,OUTPUT felmedd,OUTPUT val).
      IF felmedd NE "" THEN DO:
         MESSAGE felmedd VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
      ELSE DO:
         IF val = 1 THEN DO:                 
            {muswait.i}
            IF musz = TRUE THEN musz = FALSE.
            ASSIGN
            skrivut = FALSE.
            {AVBGOM.I}
            RUN VBYTLEVU4.W.
            {AVBFRAM.I}
            {musarrow.i}
         END.
      END.
   END.       
   APPLY "GO" TO FRAME {&FRAME-NAME}.              
   APPLY "CLOSE":U TO THIS-PROCEDURE.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEVFR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEVFR WINDOW-2
ON VALUE-CHANGED OF CMB_LEVFR IN FRAME FRAME-B /* Från leverantör */
DO:                                 
   lev1 = INPUT CMB_LEVFR.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CMB_LEVTILL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CMB_LEVTILL WINDOW-2
ON VALUE-CHANGED OF CMB_LEVTILL IN FRAME FRAME-B /* Till leverantör */
DO:                                 
   lev2 = INPUT CMB_LEVTILL.   
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
ON CLOSE OF THIS-PROCEDURE 
DO:
   IF VALID-HANDLE(bytlevapph) THEN DELETE PROCEDURE bytlevapph.
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
   {&WINDOW-NAME}:TITLE = "Lista med icketräffar för vald leverantör".   
   /*FIND FIRST huvlevtemp NO-LOCK NO-ERROR.*/
   FIND FIRST huvlevtemp WHERE huvlevtemp.DEP-NR = 999 NO-LOCK NO-ERROR. 
   IF AVAILABLE huvlevtemp THEN DO:  
      vald_kundlev = huvlevtemp.LEVKOD.
   END.                              
   ELSE DO:
      FIND FIRST levtemp WHERE levtemp.LEVKOD NE "0" 
      AND levtemp.LEVKOD NE "99" AND levtemp.BORTTAG = FALSE NO-LOCK NO-ERROR.
      vald_kundlev = levtemp.LEVKOD.
   END.
   ASSIGN         
   status-ok = CMB_LEVFR:DELETE("0")
   status-ok = CMB_LEVTILL:DELETE("0"). 
   FIND FIRST levtemp WHERE levtemp.LEVKOD = vald_kundlev
   USE-INDEX LEV NO-LOCK NO-ERROR.    
   ASSIGN
   status-ok = CMB_LEVFR:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}  
   status-ok = CMB_LEVTILL:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}
   CMB_LEVFR:SCREEN-VALUE = levtemp.LEVNAMN 
   CMB_LEVTILL:SCREEN-VALUE = levtemp.LEVNAMN
   huvudlev = levtemp.LEVNAMN. 
   FOR EACH levtemp WHERE levtemp.LEVKOD NE vald_kundlev AND 
   levtemp.LEVKOD NE "0" AND levtemp.LEVKOD NE "99" 
   AND levtemp.BORTTAG = FALSE USE-INDEX LEV NO-LOCK:
      ASSIGN      
      status-ok = CMB_LEVFR:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}
      status-ok = CMB_LEVTILL:ADD-LAST(levtemp.LEVNAMN)IN FRAME {&FRAME-NAME}.              
   END.        
   ASSIGN     
   CMB_LEVFR = INPUT CMB_LEVFR
   CMB_LEVTILL = INPUT CMB_LEVTILL
   lev1 = INPUT CMB_LEVFR
   lev2 = INPUT CMB_LEVTILL.
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
      RUN BYTLEVAPP.P PERSISTENT SET bytlevapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN BYTLEVAPP.P PERSISTENT SET bytlevapph.
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
  DISPLAY CMB_LEVFR CMB_LEVTILL 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  ENABLE CMB_LEVFR CMB_LEVTILL BTN_BEST BTN_AVB 
      WITH FRAME FRAME-B IN WINDOW WINDOW-2.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-B}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

