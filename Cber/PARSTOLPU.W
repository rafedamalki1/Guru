&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}

{PARSTOLP.I}
DEFINE VARIABLE parstolpapph AS HANDLE NO-UNDO.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS IMAGE-1 FILL-IN-1 FILL-IN-2 FILL-IN-3 ~
FILL-IN-TEXT1 FILL-IN-TEXT2 FILL-IN-TEXT3 BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-1 FILL-IN-2 FILL-IN-3 ~
FILL-IN-TEXT1 FILL-IN-TEXT2 FILL-IN-TEXT3 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(1)":U 
     VIEW-AS FILL-IN 
     SIZE 3.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS CHARACTER FORMAT "X(1)":U 
     VIEW-AS FILL-IN 
     SIZE 3.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-3 AS CHARACTER FORMAT "X(1)":U 
     VIEW-AS FILL-IN 
     SIZE 3.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TEXT1 AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS FILL-IN 
     SIZE 10.88 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TEXT2 AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS FILL-IN 
     SIZE 10.88 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TEXT3 AS CHARACTER FORMAT "X(10)":U 
     VIEW-AS FILL-IN 
     SIZE 10.88 BY 1 NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "BILDER\stolp.gif":U
     SIZE 47.13 BY 12.58.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-1 AT ROW 16.67 COL 15 COLON-ALIGNED NO-LABEL
     FILL-IN-2 AT ROW 16.67 COL 24 COLON-ALIGNED NO-LABEL
     FILL-IN-3 AT ROW 16.67 COL 33.75 COLON-ALIGNED NO-LABEL
     FILL-IN-TEXT1 AT ROW 18.63 COL 7.88 COLON-ALIGNED NO-LABEL
     FILL-IN-TEXT2 AT ROW 18.63 COL 20.88 COLON-ALIGNED NO-LABEL
     FILL-IN-TEXT3 AT ROW 18.63 COL 34 COLON-ALIGNED NO-LABEL
     BTN_OK AT ROW 20.5 COL 31.5
     BTN_AVB AT ROW 20.5 COL 46.5
     "Ange Ert val för benämning av parstolpar." VIEW-AS TEXT
          SIZE 45.5 BY 1.08 AT ROW 1.67 COL 10.5
          FONT 17
     "Ex: (V, M, H) (1, 2, 3) (A, B, C)" VIEW-AS TEXT
          SIZE 35.75 BY .88 AT ROW 2.92 COL 10.5
          FONT 17
     "Förklaringar:" VIEW-AS TEXT
          SIZE 13.63 BY .67 AT ROW 17.92 COL 10
     IMAGE-1 AT ROW 3.96 COL 13.38
     SPACE(1.11) SKIP(5.20)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Parstolpar".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON END-ERROR OF FRAME Dialog-Frame /* Parstolpar */
DO:
   IF VALID-HANDLE(parstolpapph) THEN DELETE PROCEDURE parstolpapph.
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON ENDKEY OF FRAME Dialog-Frame /* Parstolpar */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Parstolpar */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB Dialog-Frame
ON CHOOSE OF BTN_AVB IN FRAME Dialog-Frame /* Avbryt */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK Dialog-Frame
ON CHOOSE OF BTN_OK IN FRAME Dialog-Frame /* Ok */
DO:
   IF INPUT FILL-IN-1 = INPUT FILL-IN-2 THEN DO:
      MESSAGE "Parstolparnas olika ben får inte ha samma benämning."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
      RETURN NO-APPLY.
   END.
   IF INPUT FILL-IN-1 = INPUT FILL-IN-3 THEN DO:
      MESSAGE "Parstolparnas olika ben får inte ha samma benämning."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
      RETURN NO-APPLY.
   END.  
   IF INPUT FILL-IN-2 = INPUT FILL-IN-3 THEN DO:
      MESSAGE "Parstolparnas olika ben får inte ha samma benämning."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
      RETURN NO-APPLY.
   END.
   RUN btnok_UI IN parstolpapph (INPUT FILL-IN-1,INPUT FILL-IN-2,
                                 INPUT FILL-IN-3,INPUT FILL-IN-TEXT1,
                                 INPUT FILL-IN-TEXT2,INPUT FILL-IN-TEXT3).
   
   APPLY "GO" TO BTN_OK IN FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK Dialog-Frame
ON GO OF BTN_OK IN FRAME Dialog-Frame /* Ok */
DO:
   musz = FALSE.
   IF VALID-HANDLE(parstolpapph) THEN DELETE PROCEDURE parstolpapph.
   APPLY "GO" TO FRAME {&FRAME-NAME}.
   RETURN.
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
   {DIA_M_START.I}
   {muswait.i}
   {ALLSTARTDYN.I}
   EMPTY TEMP-TABLE parstolptemp NO-ERROR. 
   RUN ladda_UI IN parstolpapph (OUTPUT TABLE parstolptemp).

   FIND FIRST parstolptemp NO-LOCK NO-ERROR.
   IF AVAILABLE parstolptemp THEN DO:
      ASSIGN
      FILL-IN-1 = parstolptemp.A
      FILL-IN-2 = parstolptemp.B
      FILL-IN-3 = parstolptemp.C
      FILL-IN-TEXT1 = parstolptemp.AFORKL
      FILL-IN-TEXT2 = parstolptemp.BFORKL
      FILL-IN-TEXT3 = parstolptemp.CFORKL.
   END.
   RUN enable_UI.       
   {FRMSIZED.I}
   {musarrow.i}
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI Dialog-Frame 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose:      
  Parameters:  
  Notes:       
-------------------------------------------------------------*/           
    
   IF Guru.Konstanter:appcon THEN DO:   
      RUN PARSTOLPAPP.P PERSISTENT SET parstolpapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN PARSTOLPAPP.P PERSISTENT SET parstolpapph.
   END.  
 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY FILL-IN-1 FILL-IN-2 FILL-IN-3 FILL-IN-TEXT1 FILL-IN-TEXT2 
          FILL-IN-TEXT3 
      WITH FRAME Dialog-Frame.
  ENABLE IMAGE-1 FILL-IN-1 FILL-IN-2 FILL-IN-3 FILL-IN-TEXT1 FILL-IN-TEXT2 
         FILL-IN-TEXT3 BTN_OK BTN_AVB 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

