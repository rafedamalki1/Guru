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

  Created: 08/14/97 -  1:08 pm

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
{GLOBVAR2DEL1.I}
&Scoped-define NEW
{FAKTPLANTEMP.I}
DEFINE INPUT PARAMETER infakplannr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER fdelnrvar AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER varacont AS LOGICAL NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR faktureradtemp.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}

{SOKDEF.I}
&Scoped-define NEW
DEFINE SHARED VARIABLE fakthmth AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.  
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-A

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS SEL_FAKTTYP BTN_OK BTN_AVS 
&Scoped-Define DISPLAYED-OBJECTS SEL_FAKTTYP 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-1 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVS AUTO-END-KEY 
     LABEL "Avsluta" 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_FVE 
     LABEL "-" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_NVE 
     LABEL "+" 
     SIZE 2.5 BY .75.

DEFINE BUTTON BTN_OK AUTO-END-KEY 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE {&NEW} SHARED VARIABLE FILL-IN-TOMDAT AS DATE FORMAT "99/99/99":U 
     LABEL "Till och med datum" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE SEL_FAKTTYP AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE NO-DRAG SCROLLBAR-VERTICAL 
     SIZE 45 BY 9 TOOLTIP "Välj fakturatyp"
     BGCOLOR 8  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-A
     BTN_NVE AT ROW 1.25 COL 32.25
     FILL-IN-TOMDAT AT ROW 1.63 COL 19.38 COLON-ALIGNED
     BTN_FVE AT ROW 2.25 COL 32.25
     SEL_FAKTTYP AT ROW 3.29 COL 1.5 NO-LABEL
     BTN_OK AT ROW 12.71 COL 17.5
     BTN_AVS AT ROW 12.71 COL 32.5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 46.25 BY 13.33.


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
         TITLE              = "Faktureringstyper"
         HEIGHT             = 13.38
         WIDTH              = 46.5
         MAX-HEIGHT         = 24.75
         MAX-WIDTH          = 95.5
         VIRTUAL-HEIGHT     = 24.75
         VIRTUAL-WIDTH      = 95.5
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
/* SETTINGS FOR BUTTON BTN_FVE IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       BTN_FVE:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR BUTTON BTN_NVE IN FRAME FRAME-A
   NO-ENABLE                                                            */
ASSIGN 
       BTN_NVE:HIDDEN IN FRAME FRAME-A           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-TOMDAT IN FRAME FRAME-A
   NO-DISPLAY SHARED NO-ENABLE                                          */
ASSIGN 
       FILL-IN-TOMDAT:HIDDEN IN FRAME FRAME-A           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-1)
THEN WINDOW-1:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME WINDOW-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL WINDOW-1 WINDOW-1
ON WINDOW-CLOSE OF WINDOW-1 /* Faktureringstyper */
DO:   
   musz = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVS WINDOW-1
ON CHOOSE OF BTN_AVS IN FRAME FRAME-A /* Avsluta */
DO:
   musz = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FVE WINDOW-1
ON CHOOSE OF BTN_FVE IN FRAME FRAME-A /* - */
DO: 
   ASSIGN
   FILL-IN-TOMDAT = INPUT FILL-IN-TOMDAT.   
   FILL-IN-TOMDAT = FILL-IN-TOMDAT - 1.      
   DISPLAY FILL-IN-TOMDAT WITH FRAME {&FRAME-NAME}. 
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NVE WINDOW-1
ON CHOOSE OF BTN_NVE IN FRAME FRAME-A /* + */
DO:   
   ASSIGN
   FILL-IN-TOMDAT = INPUT FILL-IN-TOMDAT.   
   FILL-IN-TOMDAT = FILL-IN-TOMDAT + 1.        
   DISPLAY FILL-IN-TOMDAT WITH FRAME {&FRAME-NAME}.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK WINDOW-1
ON CHOOSE OF BTN_OK IN FRAME FRAME-A /* Ok */
DO:
   musz = FALSE.                
   ASSIGN   
   SEL_FAKTTYP = INPUT SEL_FAKTTYP.
   IF SEL_FAKTTYP = "" THEN DO:
      MESSAGE "Du måste välja någon faktureringstyp." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.    
   RUN slutfatyp_UI IN fakthmth 
   (INPUT infakplannr,INPUT fdelnrvar,INPUT SEL_FAKTTYP,INPUT FILL-IN-TOMDAT,OUTPUT faktureradtemp.FAKTTYPID). 
   ASSIGN
   faktureradtemp.SENASTTID = FILL-IN-TOMDAT.   
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-TOMDAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TOMDAT WINDOW-1
ON LEAVE OF FILL-IN-TOMDAT IN FRAME FRAME-A /* Till och med datum */
DO:
   ASSIGN
   FILL-IN-TOMDAT = INPUT FILL-IN-TOMDAT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-TOMDAT WINDOW-1
ON MOUSE-MENU-CLICK OF FILL-IN-TOMDAT IN FRAME FRAME-A /* Till och med datum */
DO:
   ASSIGN
   FILL-IN-TOMDAT = INPUT FILL-IN-TOMDAT
   Guru.GlobalaVariabler:regdatum = INPUT FILL-IN-TOMDAT.
   RUN AlmanBtn.w.
   FILL-IN-TOMDAT = Guru.GlobalaVariabler:regdatum.
   DISPLAY FILL-IN-TOMDAT WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SEL_FAKTTYP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SEL_FAKTTYP WINDOW-1
ON VALUE-CHANGED OF SEL_FAKTTYP IN FRAME FRAME-A
DO:
/*   MMMMMMMMMM*/
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
  musz = TRUE.
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
   ASSIGN
   SEL_FAKTTYP:FONT = Guru.Konstanter:varforetypval[20] 
   SEL_FAKTTYP:BGCOLOR = 15
   SEL_FAKTTYP:FGCOLOR = ?.
   {muswait.i}  
   FIND FIRST vfaktplantemp WHERE vfaktplantemp.FAKTNR = infakplannr NO-ERROR.
   FIND FIRST faktureradtemp WHERE faktureradtemp.FAKTNR = infakplannr AND faktureradtemp.FDELNR = fdelnrvar NO-LOCK NO-ERROR.
   /*FILL-IN-TOMDAT = FAKTURERAD.SENASTTID.      */
   RUN startfatyp_UI IN fakthmth (INPUT  infakplannr, 
                                  INPUT  fdelnrvar,                 
                                  INPUT varacont,
                                  OUTPUT TABLE valsoktemp).
   RUN momstidigare_UI.
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
  DISPLAY SEL_FAKTTYP 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  ENABLE SEL_FAKTTYP BTN_OK BTN_AVS 
      WITH FRAME FRAME-A IN WINDOW WINDOW-1.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE momstidigare_UI WINDOW-1 
PROCEDURE momstidigare_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   FOR EACH valsoktemp:
      status-ok = SEL_FAKTTYP:ADD-LAST(valsoktemp.SOKCHAR[1]) IN FRAME {&FRAME-NAME}. 
      IF valsoktemp.SOKVAL = 1 THEN SEL_FAKTTYP = valsoktemp.SOKCHAR[1].
   END.           
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

