&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME WINDOW-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WINDOW-3 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 04/21/97 -  2:05 pm

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
DEFINE INPUT PARAMETER infakplannr AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{SOKDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{FAKTPLANTEMP.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE WINDOW
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-C

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN_SLUTFAKT BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_FAKTNR FILL-IN_NAMN ~
FILL-IN_SLUTFAKT FILL-IN_TOTPRIS FILL-IN_MTRLKOST FILL-IN_OVRKOST ~
FILL-IN_BELOPP FILL-IN_OBELOPP FILL-IN_TBELOPP FILL-IN_RESKOST ~
FILL-IN_LONKOST FILL-IN_KOSTBELOPP 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WINDOW-3 AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB AUTO-GO 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN_BELOPP AS DECIMAL FORMAT "->>>>>>>9" INITIAL 0 
     LABEL "Arbetskostnad" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_FAKTNR AS INTEGER FORMAT ">>>>>>9" INITIAL 0 
     LABEL "Fakturaplannr" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .83.

DEFINE VARIABLE FILL-IN_KOSTBELOPP AS DECIMAL FORMAT "->>>>>>>9" INITIAL 0 
     LABEL "Konterat belopp" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_LONKOST AS DECIMAL FORMAT "->>>>>>>9" INITIAL 0 
     LABEL "Lönetill. kostnad" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_MTRLKOST AS DECIMAL FORMAT "->>>>>>>9" INITIAL 0 
     LABEL "Materiel kostnad" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_NAMN AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 31 BY .83.

DEFINE VARIABLE FILL-IN_OBELOPP AS DECIMAL FORMAT "->>>>>>>9" INITIAL 0 
     LABEL "Övertid kostnad" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_OVRKOST AS DECIMAL FORMAT "->>>>>>>9" INITIAL 0 
     LABEL "Övrig kostnad" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_RESKOST AS DECIMAL FORMAT "->>>>>>>9" INITIAL 0 
     LABEL "Restids kostnad" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_SLUTFAKT AS LOGICAL FORMAT "Ja/Nej" INITIAL NO 
     LABEL "Slutfakt" 
     VIEW-AS FILL-IN 
     SIZE 4 BY 1.

DEFINE VARIABLE FILL-IN_TBELOPP AS DECIMAL FORMAT "->>>>>>>9" INITIAL 0 
     LABEL "Trakt. kostnad" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN_TOTPRIS AS DECIMAL FORMAT "->>>>>>>9" INITIAL 0 
     LABEL "Totalt" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .83 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-C
     FILL-IN_FAKTNR AT ROW 1.5 COL 18 COLON-ALIGNED
     FILL-IN_NAMN AT ROW 1.5 COL 30.38 COLON-ALIGNED NO-LABEL
     FILL-IN_SLUTFAKT AT ROW 2.67 COL 18 COLON-ALIGNED
     FILL-IN_TOTPRIS AT ROW 6.46 COL 18 COLON-ALIGNED
     FILL-IN_MTRLKOST AT ROW 7.38 COL 18 COLON-ALIGNED
     FILL-IN_OVRKOST AT ROW 8.25 COL 18 COLON-ALIGNED
     FILL-IN_BELOPP AT ROW 9.17 COL 18 COLON-ALIGNED
     FILL-IN_OBELOPP AT ROW 10.08 COL 18 COLON-ALIGNED
     FILL-IN_TBELOPP AT ROW 10.96 COL 18 COLON-ALIGNED
     FILL-IN_RESKOST AT ROW 11.88 COL 18 COLON-ALIGNED
     FILL-IN_LONKOST AT ROW 12.75 COL 18 COLON-ALIGNED
     FILL-IN_KOSTBELOPP AT ROW 13.67 COL 18 COLON-ALIGNED
     BTN_AVB AT ROW 15.83 COL 49.38
     "Hittills fakturerat" VIEW-AS TEXT
          SIZE 21.13 BY .75 AT ROW 5.75 COL 15.75
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 63.75 BY 16.08.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: WINDOW
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WINDOW-3 ASSIGN
         HIDDEN             = YES
         TITLE              = "Ändrig av slutfakturering"
         HEIGHT             = 16.21
         WIDTH              = 63.88
         MAX-HEIGHT         = 25.13
         MAX-WIDTH          = 100
         VIRTUAL-HEIGHT     = 25.13
         VIRTUAL-WIDTH      = 100
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
/* SETTINGS FOR WINDOW WINDOW-3
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME FRAME-C
                                                                        */
/* SETTINGS FOR FILL-IN FILL-IN_BELOPP IN FRAME FRAME-C
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_FAKTNR IN FRAME FRAME-C
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_KOSTBELOPP IN FRAME FRAME-C
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_LONKOST IN FRAME FRAME-C
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_MTRLKOST IN FRAME FRAME-C
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN_MTRLKOST:HIDDEN IN FRAME FRAME-C           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_NAMN IN FRAME FRAME-C
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_OBELOPP IN FRAME FRAME-C
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_OVRKOST IN FRAME FRAME-C
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN_OVRKOST:HIDDEN IN FRAME FRAME-C           = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN_RESKOST IN FRAME FRAME-C
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_TBELOPP IN FRAME FRAME-C
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN_TOTPRIS IN FRAME FRAME-C
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-3)
THEN WINDOW-3:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-C
/* Query rebuild information for FRAME FRAME-C
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME FRAME-C */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME WINDOW-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL WINDOW-3 WINDOW-3
ON WINDOW-CLOSE OF WINDOW-3 /* Ändrig av slutfakturering */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-3
ON CHOOSE OF BTN_AVB IN FRAME FRAME-C /* Ok */
DO:
   APPLY "GO" TO BTN_AVB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB WINDOW-3
ON GO OF BTN_AVB IN FRAME FRAME-C /* Ok */
DO:
   APPLY "CLOSE" TO THIS-PROCEDURE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_SLUTFAKT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SLUTFAKT WINDOW-3
ON LEAVE OF FILL-IN_SLUTFAKT IN FRAME FRAME-C /* Slutfakt */
DO:
   FILL-IN_SLUTFAKT = INPUT FILL-IN_SLUTFAKT.
   IF FILL-IN_SLUTFAKT = FALSE  THEN DO:
      MESSAGE  "Vill du ta bort slutfaktureringen?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val1 AS LOGICAL.
      CASE val1:
         WHEN TRUE THEN DO:
            vfaktplantemp.SLUTFAKT = FILL-IN_SLUTFAKT.
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 84
            soktemp.SOKINT[1] = infakplannr
            soktemp.SOKLOG[1] = vfaktplantemp.SLUTFAKT.
            {SOKANROP.I}  
         END.
         WHEN FALSE THEN DO:
            
         END.      
      END CASE.                   
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_SLUTFAKT WINDOW-3
ON MOUSE-SELECT-CLICK OF FILL-IN_SLUTFAKT IN FRAME FRAME-C /* Slutfakt */
DO:
   FILL-IN_SLUTFAKT = INPUT FILL-IN_SLUTFAKT. 
   IF FILL-IN_SLUTFAKT = TRUE THEN FILL-IN_SLUTFAKT = FALSE.
   ELSE FILL-IN_SLUTFAKT = TRUE.
   DISPLAY FILL-IN_SLUTFAKT WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WINDOW-3 


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
   FIND FIRST vfaktplantemp WHERE vfaktplantemp.FAKTNR = infakplannr NO-LOCK NO-ERROR.   
   ASSIGN 
   FILL-IN_SLUTFAKT = vfaktplantemp.SLUTFAKT
   FILL-IN_FAKTNR   = vfaktplantemp.FAKTNR  
   FILL-IN_NAMN     = vfaktplantemp.NAMN.    
   RUN open_UI.   
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WINDOW-3  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WINDOW-3)
  THEN DELETE WIDGET WINDOW-3.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WINDOW-3  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN_FAKTNR FILL-IN_NAMN FILL-IN_SLUTFAKT FILL-IN_TOTPRIS 
          FILL-IN_MTRLKOST FILL-IN_OVRKOST FILL-IN_BELOPP FILL-IN_OBELOPP 
          FILL-IN_TBELOPP FILL-IN_RESKOST FILL-IN_LONKOST FILL-IN_KOSTBELOPP 
      WITH FRAME FRAME-C IN WINDOW WINDOW-3.
  ENABLE FILL-IN_SLUTFAKT BTN_AVB 
      WITH FRAME FRAME-C IN WINDOW WINDOW-3.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-C}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE open_UI WINDOW-3 
PROCEDURE open_UI :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/   
   
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 83
   soktemp.SOKINT[1] = infakplannr.
   {SOKANROP.I}      
   ASSIGN  
   FILL-IN_BELOPP     = soktemp.SOKDEC[1]
   FILL-IN_KOSTBELOPP = soktemp.SOKDEC[2]
   FILL-IN_LONKOST    = soktemp.SOKDEC[3]
   FILL-IN_RESKOST    = soktemp.SOKDEC[4]
   FILL-IN_OBELOPP    = soktemp.SOKDEC[5]
   FILL-IN_TBELOPP    = soktemp.SOKINT[2]  
   FILL-IN_OVRKOST    = soktemp.SOKINT[3]
   FILL-IN_MTRLKOST   = soktemp.SOKINT[4].
   ASSIGN
   FILL-IN_TOTPRIS = FILL-IN_TOTPRIS + FILL-IN_BELOPP + FILL-IN_KOSTBELOPP + 
   FILL-IN_LONKOST + FILL-IN_OBELOPP + FILL-IN_TBELOPP + FILL-IN_OVRKOST + 
   FILL-IN_MTRLKOST + FILL-IN_RESKOST.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

