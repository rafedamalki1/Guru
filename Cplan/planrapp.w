&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
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
{AVTPLANTEMP.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{REGVAR.I}

&Scoped-define NEW NEW
DEFINE NEW SHARED VARIABLE vallista AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE vartpro AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE vart AS CHARACTER FORMAT "X(3)" NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO. 
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE hjdelvar AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS SEL_UPP BTN_VISA BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS SEL_UPP 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avsluta":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_VISA 
     LABEL "Visa" 
     SIZE 14 BY 1.

DEFINE {&NEW} SHARED VARIABLE RAD_PERIOD AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Visning per år", 1,
"Visning per period", 2
     SIZE 21.5 BY 2.75 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE SEL_UPP AS CHARACTER INITIAL ? 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 39.5 BY 9.5 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     RAD_PERIOD AT ROW 1.25 COL 19.88 NO-LABEL
     SEL_UPP AT ROW 4.17 COL 1.5 NO-LABEL
     BTN_VISA AT ROW 6.21 COL 42
     BTN_AVB AT ROW 13.88 COL 42
     "Rapporter:" VIEW-AS TEXT
          SIZE 12.25 BY 1.25 AT ROW 1.25 COL 1.75
          FONT 17
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 56.13 BY 14.17.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-1 ASSIGN
         HIDDEN             = YES
         TITLE              = "Rapporter"
         HEIGHT             = 14.21
         WIDTH              = 56.5
         MAX-HEIGHT         = 27.25
         MAX-WIDTH          = 100
         VIRTUAL-HEIGHT     = 27.25
         VIRTUAL-WIDTH      = 100
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
/* SETTINGS FOR RADIO-SET RAD_PERIOD IN FRAME FRAME-A
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       RAD_PERIOD:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR SELECTION-LIST SEL_UPP IN FRAME FRAME-A
   SHARED                                                               */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-1
ON CHOOSE OF BTN_AVB IN FRAME FRAME-A /* Avsluta */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_VISA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_VISA WINDOW-1
ON CHOOSE OF BTN_VISA IN FRAME FRAME-A /* Visa */
DO:       
 
   RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_UPP WINDOW-1
ON MOUSE-SELECT-DBLCLICK OF SEL_UPP IN FRAME FRAME-A
DO:
   RUN visa_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_UPP WINDOW-1
ON VALUE-CHANGED OF SEL_UPP IN FRAME FRAME-A
DO:
   SEL_UPP = INPUT SEL_UPP.
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
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

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
   IF Guru.Konstanter:appcon THEN DO:
      RUN KBENHMT.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (OUTPUT TABLE kbenamntemp).
   END.
   ELSE DO:
      RUN KBENHMT.P
      (OUTPUT TABLE kbenamntemp).
   END.
   FIND FIRST kbenamntemp USE-INDEX KBEN NO-LOCK NO-ERROR.
   status-ok = SEL_UPP:ADD-LAST(Guru.Konstanter:gplk + " - konto").
   status-ok = SEL_UPP:ADD-LAST("Budget - konto").
   status-ok = SEL_UPP:ADD-LAST(CAPS(SUBSTRING(kbenamntemp.K1,1,1)) + LC(SUBSTRING(kbenamntemp.K1,2)) + " - " + CAPS(SUBSTRING(kbenamntemp.K2,1,1)) + LC(SUBSTRING(kbenamntemp.K2,2))).
   /*
   status-ok = SEL_UPP:ADD-LAST("Organisation - objekt").
   */
   status-ok = SEL_UPP:ADD-LAST(Guru.Konstanter:gomrk + " - " + LC(Guru.Konstanter:gplk)).
/*    status-ok = SEL_UPP:ADD-LAST("Inaktiva " + gomrk + " - " + LC(Guru.Konstanter:gplk)).  */
   status-ok = SEL_UPP:ADD-LAST("Generella rapporter").
   RUN enable_UI.   
   {FRMSIZE.I}  
   APPLY "ENTRY" TO SEL_UPP IN FRAME {&FRAME-NAME}.  
   {musarrow.i}    
   {WIN_M_SLUT.I}
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY SEL_UPP 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE SEL_UPP BTN_VISA BTN_AVB 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE visa_UI WINDOW-1 
PROCEDURE visa_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
   {muswait.i}     
   {AVBGOM.I}
   IF SEL_UPP = Guru.Konstanter:gplk + " - konto" THEN DO:
      vallista = 1.
      RUN KONTVAL1.W.      
   END.  
   IF SEL_UPP = "Budget - konto" THEN DO:
      vallista = 2.
      RUN KONTVAL1.W.     
   END.
   /*
   IF SEL_UPP = "Organisation - objekt" THEN DO:
      vallista = 3.
      RUN KONTVAL2.W.      
   END.
   */
   IF SEL_UPP = CAPS(SUBSTRING(kbenamntemp.K1,1,1)) + LC(SUBSTRING(kbenamntemp.K1,2)) + " - " + CAPS(SUBSTRING(kbenamntemp.K2,1,1)) + LC(SUBSTRING(kbenamntemp.K2,2)) THEN DO:
      vallista = 3.
      RUN KONTVAL2.W.      
   END.
   IF SEL_UPP = "Generella rapporter" THEN DO:
      RUN GENRAPP.W.
   END.
   IF SEL_UPP = Guru.Konstanter:gomrk + " - " + LC(Guru.Konstanter:gplk) THEN DO:
      RUN OMRPLAN.W (INPUT 0).
   END.
/*    IF SEL_UPP = "Inaktiva " + gomrk + " - " + LC(Guru.Konstanter:gplk) THEN DO: */
/*       RUN OMRPLAN.W (INPUT 1).                                  */
/*    END.                                                         */
   
   {AVBFRAM.I}
   musz = FALSE.
   {musarrow.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

