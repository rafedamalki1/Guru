&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Vacation-Info
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Vacation-Info 
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

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER schedOCX AS COM-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER iEmp     AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER iVac     AS INTEGER NO-UNDO.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Vacation-Info

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_OK FILL-IN-DESCRIPTION Btn_Cancel ~
FILL-IN-FROM FILL-IN-TO 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-DESCRIPTION FILL-IN-FROM ~
FILL-IN-TO 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN-DESCRIPTION AS CHARACTER FORMAT "X(256)":U 
     LABEL "Description" 
     VIEW-AS FILL-IN 
     SIZE 33 BY 1 TOOLTIP "Description" NO-UNDO.

DEFINE VARIABLE FILL-IN-FROM AS DATE FORMAT "99/99/99":U 
     LABEL "From (Date):" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 TOOLTIP "From" NO-UNDO.

DEFINE VARIABLE FILL-IN-TO AS DATE FORMAT "99/99/99":U 
     LABEL "To (Date):" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 TOOLTIP "To" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Vacation-Info
     Btn_OK AT ROW 1.52 COL 49
     FILL-IN-DESCRIPTION AT ROW 1.71 COL 11 COLON-ALIGNED
     Btn_Cancel AT ROW 2.76 COL 49
     FILL-IN-FROM AT ROW 4.57 COL 19 COLON-ALIGNED
     FILL-IN-TO AT ROW 6.24 COL 19 COLON-ALIGNED
     "Begin/End:" VIEW-AS TEXT
          SIZE 14 BY .71 AT ROW 3.38 COL 3
     SPACE(48.19) SKIP(4.09)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Vacation Information"
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
/* SETTINGS FOR DIALOG-BOX Dialog-Vacation-Info
                                                                        */
ASSIGN 
       FRAME Dialog-Vacation-Info:SCROLLABLE       = FALSE
       FRAME Dialog-Vacation-Info:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Vacation-Info
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Vacation-Info Dialog-Vacation-Info
ON ENTRY OF FRAME Dialog-Vacation-Info /* Vacation Information */
DO:
    IF iVac >= 0 THEN DO:
        fill-in-description:SCREEN-VALUE = schedOCX:TBUser2(iVac).
        fill-in-from:SCREEN-VALUE = schedOCX:TBBeg(iVac).
        fill-in-to:SCREEN-VALUE = schedOCX:TBEnd(iVac).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Vacation-Info Dialog-Vacation-Info
ON WINDOW-CLOSE OF FRAME Dialog-Vacation-Info /* Vacation Information */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Vacation-Info
ON CHOOSE OF Btn_OK IN FRAME Dialog-Vacation-Info /* OK */
DO:
  DEFINE VARIABLE iTB   AS INTEGER.
  DEFINE VARIABLE sUser AS CHARACTER.
  DEFINE VARIABLE s     AS CHARACTER.
    
  IF iVac = -1 THEN DO: 
    schedOCX:TBMax = schedOCX:TBMax + 1.
    iTB = schedOCX:TBMax - 1.
  END.
  ELSE DO:
    iTB = iVac.
  END.
  
  schedOCX:TBResource( iTB ) = iEmp.
  schedOCX:TBUser2( iTB ) = fill-in-description:SCREEN-VALUE.
  schedOCX:TBBeg( iTB ) = fill-in-from:SCREEN-VALUE.
  schedOCX:TBEnd( iTB ) = fill-in-to:SCREEN-VALUE.
  sUser = schedOCX:ResName( iEmp ) + " - " + fill-in-description:SCREEN-VALUE.
  schedOCX:TBUser1( iTB ) = sUser.
  
  s = schedOCX:TBUser2(iTB).
  s = s + " From:" + schedOCX:TBBeg(iTB).
  s = s + " to:" + schedOCX:TBEnd(iTB).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Vacation-Info 


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Vacation-Info _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Vacation-Info.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Vacation-Info _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-DESCRIPTION FILL-IN-FROM FILL-IN-TO 
      WITH FRAME Dialog-Vacation-Info.
  ENABLE Btn_OK FILL-IN-DESCRIPTION Btn_Cancel FILL-IN-FROM FILL-IN-TO 
      WITH FRAME Dialog-Vacation-Info.
  VIEW FRAME Dialog-Vacation-Info.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Vacation-Info}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


