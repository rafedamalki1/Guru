&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 03/31/97 -  8:34 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */ 
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
&Scoped-define SHARED SHARED
{ANNAMN.I}
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE tabandapph AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-TAB1 FILL-IN-TAB2 FILL-IN-TAB3 ~
BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-TAB1 FILL-IN-TAB2 FILL-IN-TAB3 

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

DEFINE VARIABLE FILL-IN-TAB1 AS CHARACTER FORMAT "X(25)":U 
     LABEL "Tabell 1" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TAB2 AS CHARACTER FORMAT "X(25)":U 
     LABEL "Tabell 2" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TAB3 AS CHARACTER FORMAT "X(25)":U 
     LABEL "Tabell 3" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     FILL-IN-TAB1 AT ROW 2 COL 12 COLON-ALIGNED
     FILL-IN-TAB2 AT ROW 4 COL 12 COLON-ALIGNED
     FILL-IN-TAB3 AT ROW 6 COL 12 COLON-ALIGNED
     BTN_OK AT ROW 7.75 COL 11
     BTN_AVB AT ROW 7.75 COL 26
     SPACE(1.24) SKIP(0.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ändring av tabellnamn".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Ändring av tabellnamn */
DO:
   IF VALID-HANDLE(tabandapph) THEN DELETE PROCEDURE tabandapph.
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Ändring av tabellnamn */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB DIALOG-1
ON CHOOSE OF BTN_AVB IN FRAME DIALOG-1 /* Avbryt */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:  
   {muswait.i} 
   ASSIGN
   FILL-IN-TAB1 = INPUT FILL-IN-TAB1
   FILL-IN-TAB2 = INPUT FILL-IN-TAB2
   FILL-IN-TAB3 = INPUT FILL-IN-TAB3
   felmedd = "".
   RUN btnok_UI IN tabandapph (INPUT FILL-IN-TAB1,
                               INPUT FILL-IN-TAB2,
                               INPUT FILL-IN-TAB3,
                               OUTPUT felmedd).
   IF felmedd NE "" THEN DO:
      MESSAGE felmedd VIEW-AS ALERT-BOX TITLE "Meddelande".
      felmedd = "".
      RETURN NO-APPLY.
   END.
   ELSE DO:
      FIND FIRST annamntemp NO-LOCK NO-ERROR.
      ASSIGN 
      annamntemp.TAB1 = INPUT FILL-IN-TAB1
      annamntemp.TAB2 = INPUT FILL-IN-TAB2
      annamntemp.TAB3 = INPUT FILL-IN-TAB3.
      APPLY "GO" TO BTN_OK IN FRAME {&FRAME-NAME}.
   END.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON GO OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:
   IF VALID-HANDLE(tabandapph) THEN DELETE PROCEDURE tabandapph.
   musz = FALSE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-1 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Add Trigger to equate WINDOW-CLOSE to END-ERROR                      */
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK: 
   {DIA_M_START.I}
   {ALLSTARTDYN.I}
   FIND FIRST annamntemp NO-LOCK NO-ERROR.
   ASSIGN
   FILL-IN-TAB1 = annamntemp.TAB1
   FILL-IN-TAB2 = annamntemp.TAB2
   FILL-IN-TAB3 = annamntemp.TAB3.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE allstartbrw_UI DIALOG-1 
PROCEDURE allstartbrw_UI :
/* -----------------------------------------------------------
  Purpose:      
  Parameters:  
  Notes:       
-------------------------------------------------------------*/ 
   IF Guru.Konstanter:appcon THEN DO:
      RUN TABANDAPP.P PERSISTENT SET tabandapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN TABANDAPP.P PERSISTENT SET tabandapph.
   END.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-1  _DEFAULT-DISABLE
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
  HIDE FRAME DIALOG-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-1  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-TAB1 FILL-IN-TAB2 FILL-IN-TAB3 
      WITH FRAME DIALOG-1.
  ENABLE FILL-IN-TAB1 FILL-IN-TAB2 FILL-IN-TAB3 BTN_OK BTN_AVB 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

