&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: acgendlg.w

  Description: Generate report using acgenrpt.p

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Gerry Seidl  

  Created: July 1, 1997
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{samples/actuate/acpproc.i} /* Actuate API Preprocessor values */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS pc_filename btn_Browse tog_launch tog_print ~
tog_instance rect_options tog_async tog_generate tog_hide tog_view tog_open ~
Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS pc_filename tog_launch tog_print ~
tog_instance tog_async tog_generate tog_hide tog_view tog_open 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_Browse 
     LABEL "Browse..." 
     SIZE 11 BY 1.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE pc_filename AS CHARACTER FORMAT "X(256)":U 
     LABEL "Filename" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1 NO-UNDO.

DEFINE RECTANGLE rect_options
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 63 BY 4.33.

DEFINE VARIABLE tog_async AS LOGICAL INITIAL no 
     LABEL "Run report asynchronously" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

DEFINE VARIABLE tog_generate AS LOGICAL INITIAL yes 
     LABEL "Generate report" 
     VIEW-AS TOGGLE-BOX
     SIZE 30 BY .81 NO-UNDO.

DEFINE VARIABLE tog_hide AS LOGICAL INITIAL no 
     LABEL "Do not display EUD" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE tog_instance AS LOGICAL INITIAL no 
     LABEL "Use current instance of EUD" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81 NO-UNDO.

DEFINE VARIABLE tog_launch AS LOGICAL INITIAL no 
     LABEL "Always launch new EUD" 
     VIEW-AS TOGGLE-BOX
     SIZE 32 BY .81 NO-UNDO.

DEFINE VARIABLE tog_open AS LOGICAL INITIAL no 
     LABEL "Keep EUD open" 
     VIEW-AS TOGGLE-BOX
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE tog_print AS LOGICAL INITIAL no 
     LABEL "Print generated report" 
     VIEW-AS TOGGLE-BOX
     SIZE 26 BY .81 NO-UNDO.

DEFINE VARIABLE tog_view AS LOGICAL INITIAL no 
     LABEL "Display generated report" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     pc_filename AT ROW 1.48 COL 10 COLON-ALIGNED
     btn_Browse AT ROW 1.48 COL 54
     tog_launch AT ROW 3.62 COL 3
     tog_print AT ROW 3.62 COL 35
     tog_instance AT ROW 4.57 COL 3
     tog_async AT ROW 4.57 COL 35
     tog_generate AT ROW 5.52 COL 3
     tog_hide AT ROW 5.52 COL 35
     tog_view AT ROW 6.43 COL 3
     tog_open AT ROW 6.43 COL 35
     Btn_OK AT ROW 1.48 COL 67
     Btn_Cancel AT ROW 2.76 COL 67
     "Generate options" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 2.91 COL 3
     rect_options AT ROW 3.14 COL 2
     SPACE(18.39) SKIP(0.09)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Generate Report"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   Custom                                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Generate Report */
DO:
  DEFINE VARIABLE pi_Status    AS INTEGER   NO-UNDO.
  DEFINE VARIABLE pi_Options   AS INTEGER   NO-UNDO.
  
  /* Assign fields from the screen */
  ASSIGN pc_filename
         tog_async 
         tog_open 
         tog_instance 
         tog_generate
         tog_view
         tog_launch 
         tog_hide 
         tog_print.
         
  /* Assemble report options */
  IF tog_launch   THEN pi_Options = pi_Options + {&AC_REQ_LAUNCH_ALWAYS}.
  IF tog_instance THEN pi_Options = pi_Options + {&AC_REQ_LOCAL}.
  IF tog_generate THEN pi_Options = pi_Options + {&AC_REQ_GENERATE}.
  IF tog_view     THEN pi_Options = pi_Options + {&AC_REQ_VIEW}.
  IF tog_print    THEN pi_Options = pi_Options + {&AC_REQ_PRINT}.
  IF tog_async    THEN pi_Options = pi_Options + {&AC_REQ_ASYNC}.
  IF tog_hide     THEN pi_Options = pi_Options + {&AC_REQ_HIDE}.  
  IF tog_open     THEN pi_Options = pi_Options + {&AC_REQ_STAY_OPEN}.

  /* Call procedure to generate report */
  RUN samples/actuate/acgenrpt.p (INPUT  pc_filename,
                                  INPUT  ?,
                                  INPUT  pi_Options,
                                  OUTPUT pi_Status).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Generate Report */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_Browse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Browse Dialog-Frame
ON CHOOSE OF btn_Browse IN FRAME Dialog-Frame /* Browse... */
DO:
  DEFINE VAR l_ok              AS LOGICAL             NO-UNDO.
  DEFINE VAR filename          AS CHARACTER           NO-UNDO.
  DEFINE VAR Filter_NameString AS CHARACTER EXTENT 3  NO-UNDO.
  DEFINE VAR Filter_FileSpec   LIKE Filter_NameString NO-UNDO.

  /* Initialize the file filters, for special cases. */
  ASSIGN Filter_NameString[ 1 ] = "All parameter files (*.rov~;*.rox)"
         Filter_FileSpec[ 1 ]   = "*.rov~;*.rox"
         Filter_NameString[ 2 ] = "Report Executable (*.rox)"
         Filter_FileSpec[ 2 ]   = "*.rox"
         Filter_NameString[ 3 ] = "Report parameter values (*.rov)"
         Filter_FileSpec[ 3 ]   = "*.rov". 

  /* Ask for a file name. NOTE: File-names to run must exist */                          
  filename = pc_filename:SCREEN-VALUE.
  SYSTEM-DIALOG GET-FILE filename
      TITLE    "Run report"
      FILTERS  Filter_NameString[ 1 ]   Filter_FileSpec[ 1 ],
               Filter_NameString[ 2 ]   Filter_FileSpec[ 2 ],
               Filter_NameString[ 3 ]   Filter_FileSpec[ 3 ]
      MUST-EXIST
      UPDATE   l_ok IN WINDOW {&WINDOW-NAME}.  
  IF l_ok THEN DO:
    pc_filename:SCREEN-VALUE = filename.
    APPLY "RETURN" TO pc_filename.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  ASSIGN pc_filename.
  IF pc_filename EQ ? OR pc_filename EQ "" THEN DO:
    MESSAGE "You must enter a ROX or ROV filename." VIEW-AS ALERT-BOX ERROR.
    APPLY "ENTRY" TO pc_filename.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame _DEFAULT-ENABLE
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
  DISPLAY pc_filename tog_launch tog_print tog_instance tog_async tog_generate 
          tog_hide tog_view tog_open 
      WITH FRAME Dialog-Frame.
  ENABLE pc_filename btn_Browse tog_launch tog_print tog_instance rect_options 
         tog_async tog_generate tog_hide tog_view tog_open Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


