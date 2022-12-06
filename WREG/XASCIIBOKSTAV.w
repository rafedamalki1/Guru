&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: XASCIIBOKSTAV.w

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-text AscIIvarde Klartext Btn_Visa 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-text AscIIvarde Klartext 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AsscIISvar Dialog-Frame 
FUNCTION AsscIISvar RETURNS CHARACTER
  (INPUT asIIint AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Visa AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.13
     BGCOLOR 8 .

DEFINE VARIABLE AscIIvarde AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 44.5 BY 15.5 NO-UNDO.

DEFINE VARIABLE Klartext AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 44.5 BY 15.5 NO-UNDO.

DEFINE VARIABLE FILL-IN-text AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 91 BY 1.5 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-text AT ROW 1.5 COL 5.5 COLON-ALIGNED NO-LABEL WIDGET-ID 6
     AscIIvarde AT ROW 3.25 COL 4 NO-LABEL WIDGET-ID 2
     Klartext AT ROW 3.75 COL 63 NO-LABEL WIDGET-ID 4
     Btn_Visa AT ROW 24.25 COL 86
     SPACE(12.49) SKIP(2.36)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "<insert dialog title>"
         DEFAULT-BUTTON Btn_Visa WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* <insert dialog title> */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME AscIIvarde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL AscIIvarde Dialog-Frame
ON LEAVE OF AscIIvarde IN FRAME Dialog-Frame
DO:
   DEFINE VARIABLE hh AS CHARACTER NO-UNDO.
   
   AscIIvarde = INPUT AscIIvarde.
   RUN Klarttext_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Visa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Visa Dialog-Frame
ON CHOOSE OF Btn_Visa IN FRAME Dialog-Frame /* OK */
DO:
  /* MES*/ 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-text
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-text Dialog-Frame
ON ENTRY OF FILL-IN-text IN FRAME Dialog-Frame
DO:
   FILL-IN-text = FILL-IN-text.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-text Dialog-Frame
ON LEAVE OF FILL-IN-text IN FRAME Dialog-Frame
DO:
   FILL-IN-text = INPUT FILL-IN-text.
   RUN Prott_UI (INPUT FILL-IN-text). 
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
  APPLY "ENTRY" TO FILL-IN-text.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-text AscIIvarde Klartext 
      WITH FRAME Dialog-Frame.
  ENABLE FILL-IN-text AscIIvarde Klartext Btn_Visa 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Klarttext_UI Dialog-Frame 
PROCEDURE Klarttext_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE hh AS CHARACTER NO-UNDO.
   DEFINE VARIABLE itxt AS INTEGER NO-UNDO.
   DEFINE VARIABLE itxt2 AS INTEGER NO-UNDO.
   itxt = 1.
   itxt2 = 1. 
   Klartext = "".
   itxt = 1.
   itxt2 = 1. 
   Klartext = "".
   REPEAT:
      IF INDEX(AscIIvarde,")",itxt) = 0 THEN LEAVE.
      itxt2 = INDEX(AscIIvarde,")",itxt).
      hh = SUBSTRING(AscIIvarde,itxt,itxt2 - itxt).
      hh = REPLACE(hh,"chr(","").
      Klartext = Klartext + CHR(integer(hh)). 
      itxt = itxt2 + 1.
   END.    
   DISPLAY AscIIvarde Klartext WITH FRAME  {&FRAME-NAME} . 
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Prott_UI Dialog-Frame 
PROCEDURE Prott_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER intxt AS CHARACTER NO-UNDO.
   DEFINE VARIABLE hh AS CHARACTER NO-UNDO.
   DEFINE VARIABLE itxt AS INTEGER NO-UNDO.
   itxt = 0.
   intxt = TRIM(intxt). 
   Klartext = "".
   AscIIvarde = "".   
   REPEAT:
      itxt = itxt + 1. 
      IF itxt > LENGTH(intxt) THEN LEAVE. 
      AscIIvarde = AscIIvarde  + "CHR(" + STRING(ASC(SUBSTRING(intxt,itxt,1))) + ") " .
      /*
      Klartext = Klartext +  AsscIISvar(ASC(SUBSTRING(intxt,itxt,1))).
      */
      Klartext = Klartext +  CHR(ASC(SUBSTRING(intxt,itxt,1))).
      IF itxt < LENGTH(intxt) THEN AscIIvarde = AscIIvarde + "+ ".
   END.    
   DISPLAY AscIIvarde Klartext WITH FRAME  {&FRAME-NAME} . 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AsscIISvar Dialog-Frame 
FUNCTION AsscIISvar RETURNS CHARACTER
  (INPUT asIIint AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  RETURN STRING(CHR(asIIint)).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

