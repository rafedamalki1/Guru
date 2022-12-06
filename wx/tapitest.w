&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE RetValue AS INTEGER NO-UNDO.
DEFINE VARIABLE TAPIERR_NOREQUESTRECIPIENT AS INTEGER NO-UNDO.
DEFINE VARIABLE TAPIERR_INVALDESTADDRESS AS INTEGER NO-UNDO.
DEFINE VARIABLE TAPIERR_REQUESTQUEUEFULL AS INTEGER NO-UNDO.
DEFINE VARIABLE TAPIERR_INVALPOINTER  AS INTEGER NO-UNDO.
DEFINE VARIABLE hCall AS HANDLE NO-UNDO.
{TAPI.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-NR FILL-IN-NAMN BTN_RING BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-NR FILL-IN-NAMN 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avsluta" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_RING 
     LABEL "Ring upp" 
     SIZE 12 BY 1.

DEFINE VARIABLE FILL-IN-NAMN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Namn" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-NR AS CHARACTER FORMAT "X(256)":U 
     LABEL "Telefonnr" 
     VIEW-AS FILL-IN 
     SIZE 22.5 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-NR AT ROW 1.5 COL 10.5 COLON-ALIGNED
     FILL-IN-NAMN AT ROW 2.75 COL 10.5 COLON-ALIGNED
     BTN_RING AT ROW 4.25 COL 12.5
     BTN_AVB AT ROW 4.25 COL 27.5
     SPACE(17.12) SKIP(0.37)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "<insert dialog title>".


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
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON END-ERROR OF FRAME Dialog-Frame /* <insert dialog title> */
DO:
  RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON ENDKEY OF FRAME Dialog-Frame /* <insert dialog title> */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* <insert dialog title> */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB Dialog-Frame
ON CHOOSE OF BTN_AVB IN FRAME Dialog-Frame /* Avsluta */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_RING
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_RING Dialog-Frame
ON CHOOSE OF BTN_RING IN FRAME Dialog-Frame /* Ring upp */
DO:
   ASSIGN
  FILL-IN-NR = INPUT FILL-IN-NR
  FILL-IN-NAMN = INPUT FILL-IN-NAMN.
  run tapi_UI (INPUT FILL-IN-NR,INPUT FILL-IN-NAMN).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-NAMN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-NAMN Dialog-Frame
ON LEAVE OF FILL-IN-NAMN IN FRAME Dialog-Frame /* Namn */
DO:
  FILL-IN-NAMN = INPUT FILL-IN-NAMN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-NR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-NR Dialog-Frame
ON LEAVE OF FILL-IN-NR IN FRAME Dialog-Frame /* Telefonnr */
DO:
  FILL-IN-NR = INPUT FILL-IN-NR.
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
   ASSIGN
   TAPIERR_NOREQUESTRECIPIENT = -2 
   TAPIERR_INVALDESTADDRESS = -4
   TAPIERR_REQUESTQUEUEFULL = -3
   TAPIERR_INVALPOINTER = -18.
   
  RUN enable_UI.
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
  DISPLAY FILL-IN-NR FILL-IN-NAMN 
      WITH FRAME Dialog-Frame.
  ENABLE FILL-IN-NR FILL-IN-NAMN BTN_RING BTN_AVB 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tapistatus_UI Dialog-Frame 
PROCEDURE tapistatus_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER lpszRetValue AS INTEGER NO-UNDO.
   IF lpszRetValue = TAPIERR_NOREQUESTRECIPIENT THEN DO:
      MESSAGE "No req" VIEW-AS ALERT-BOX.
   END.
   ELSE IF lpszRetValue = TAPIERR_INVALDESTADDRESS THEN DO:
      MESSAGE "Inv adr" VIEW-AS ALERT-BOX.
   END.
   ELSE IF lpszRetValue = TAPIERR_REQUESTQUEUEFULL THEN DO:
      MESSAGE "Queue full" VIEW-AS ALERT-BOX.
   END.
   ELSE IF lpszRetValue = TAPIERR_INVALPOINTER THEN DO:
      MESSAGE "Inval p" VIEW-AS ALERT-BOX.
   END.
   ELSE DO:
      MESSAGE "OK, connected" VIEW-AS ALERT-BOX.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE tapi_UI Dialog-Frame 
PROCEDURE tapi_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER TelNoStr AS CHARACTER NO-UNDO. 
   DEFINE INPUT PARAMETER TelNoName AS CHARACTER NO-UNDO.
   DEFINE VARIABLE GetLine AS CHARACTER NO-UNDO.
   DEFINE VARIABLE TelNo AS MEMPTR NO-UNDO.
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   DEFINE VARIABLE lpszRetValue AS INTEGER NO-UNDO.
   DEFINE VARIABLE dwSelect AS INTEGER NO-UNDO.
   DEFINE VARIABLE lpDeviceID AS CHARACTER NO-UNDO.
   
   GET-KEY-VALUE SECTION "Modem":U KEY "GetLine":U VALUE GetLine.
   IF GetLine = "?":U OR GetLine = ? THEN GetLine = "":U.
   SET-SIZE(TelNo) = LENGTH(GetLine + TelNoStr) + 1.
   DO i = 1 TO LENGTH(TelNoStr + GetLine): 
      PUT-BYTE(TelNo,i) = ASC(SUBSTRING(GetLine + TelNoStr,i,1)).
   END.
   /*PUT-BYTE(TelNo,i) = 0.*/
   RUN tapiRequestMakeCall (INPUT TelNo, 
                           INPUT "0":U, 
                           INPUT TelNoName, 
                           INPUT "0":U,
                           OUTPUT lpszRetValue).
   RUN tapistatus_UI (INPUT lpszRetValue).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

