&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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

  Created: 95/05/03 -  2:05 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER togberkop AS LOGICAL NO-UNDO. 
DEFINE OUTPUT PARAMETER togkalkop AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER togkoppkop AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER togtidlkop AS LOGICAL NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}

DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS TOG_TIDL TOG_BERKOP TOG_KALKOP TOG_KOPPKOP ~
BTN_OK FILL-IN_TEXT1 FILL-IN_TEXT2 FILL-IN_TEXT3 FILL-IN_TEXT4 
&Scoped-Define DISPLAYED-OBJECTS TOG_TIDL TOG_BERKOP TOG_KALKOP TOG_KOPPKOP ~
FILL-IN_TEXT1 FILL-IN_TEXT2 FILL-IN_TEXT3 FILL-IN_TEXT4 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_OK AUTO-GO 
     LABEL "Ok":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN_TEXT1 AS CHARACTER FORMAT "X(256)":U INITIAL "Vill ni från det gamla numret?" 
      VIEW-AS TEXT 
     SIZE 48 BY 1.21
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN_TEXT2 AS CHARACTER FORMAT "X(256)":U INITIAL "Eller vill ni?" 
      VIEW-AS TEXT 
     SIZE 48 BY 1.21
     FONT 17 NO-UNDO.

DEFINE VARIABLE FILL-IN_TEXT3 AS CHARACTER FORMAT "X(256)":U INITIAL "och markvärdering till det nya numret." 
      VIEW-AS TEXT 
     SIZE 48 BY .63 NO-UNDO.

DEFINE VARIABLE FILL-IN_TEXT4 AS CHARACTER FORMAT "X(256)":U INITIAL "OBS tidregistrering och fakturering flyttas EJ." 
      VIEW-AS TEXT 
     SIZE 48 BY .63 NO-UNDO.

DEFINE VARIABLE TOG_BERKOP AS LOGICAL INITIAL no 
     LABEL "Kopiera beredning." 
     VIEW-AS TOGGLE-BOX
     SIZE 30.5 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_KALKOP AS LOGICAL INITIAL no 
     LABEL "Kopiera kalkyler." 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_KOPPKOP AS LOGICAL INITIAL no 
     LABEL "Flytta kopplingar för beredning, kalkyler" 
     VIEW-AS TOGGLE-BOX
     SIZE 44.5 BY .79 NO-UNDO.

DEFINE VARIABLE TOG_TIDL AS LOGICAL INITIAL no 
     LABEL "Kopiera tidlägen." 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .79 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     TOG_TIDL AT ROW 3 COL 4
     TOG_BERKOP AT ROW 4.04 COL 4
     TOG_KALKOP AT ROW 5.13 COL 4
     TOG_KOPPKOP AT ROW 8 COL 4
     BTN_OK AT ROW 11.21 COL 51.38
     FILL-IN_TEXT1 AT ROW 1.54 COL 4 COLON-ALIGNED NO-LABEL
     FILL-IN_TEXT2 AT ROW 6.5 COL 4 COLON-ALIGNED NO-LABEL
     FILL-IN_TEXT3 AT ROW 9.08 COL 4 COLON-ALIGNED NO-LABEL
     FILL-IN_TEXT4 AT ROW 10 COL 4 COLON-ALIGNED NO-LABEL
     SPACE(11.99) SKIP(1.99)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Kopiera eller koppla beroenden":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

ASSIGN 
       FILL-IN_TEXT1:READ-ONLY IN FRAME DIALOG-1        = TRUE.

ASSIGN 
       FILL-IN_TEXT2:READ-ONLY IN FRAME DIALOG-1        = TRUE.

ASSIGN 
       FILL-IN_TEXT3:READ-ONLY IN FRAME DIALOG-1        = TRUE.

ASSIGN 
       FILL-IN_TEXT4:READ-ONLY IN FRAME DIALOG-1        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON END-ERROR OF FRAME DIALOG-1 /* Kopiera eller koppla beroenden */
DO:
   musz = TRUE.
   RETURN. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Kopiera eller koppla beroenden */
DO:
   musz = TRUE.
   RETURN. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:
   ASSIGN 
   togberkop = TOG_BERKOP 
   togkalkop = TOG_KALKOP 
   togkoppkop = TOG_KOPPKOP
   togtidlkop = TOG_TIDL.
   musz = FALSE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_BERKOP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_BERKOP DIALOG-1
ON VALUE-CHANGED OF TOG_BERKOP IN FRAME DIALOG-1 /* Kopiera beredning. */
DO:
   TOG_BERKOP = INPUT TOG_BERKOP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_KALKOP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_KALKOP DIALOG-1
ON VALUE-CHANGED OF TOG_KALKOP IN FRAME DIALOG-1 /* Kopiera kalkyler. */
DO:
   TOG_KALKOP = INPUT TOG_KALKOP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_KOPPKOP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_KOPPKOP DIALOG-1
ON VALUE-CHANGED OF TOG_KOPPKOP IN FRAME DIALOG-1 /* Flytta kopplingar för beredning, kalkyler */
DO:
   TOG_KOPPKOP = INPUT TOG_KOPPKOP.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_TIDL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TIDL DIALOG-1
ON VALUE-CHANGED OF TOG_TIDL IN FRAME DIALOG-1 /* Kopiera tidlägen. */
DO:
   TOG_TIDL = INPUT TOG_TIDL.
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
   RUN enable_UI.      
   {FRMSIZED.I}
   /*Vid ändring av globforetag här - gör ändring under "procedure aokopia" i nyttaoapp.p*/ 
   IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "NAEK" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:
      ASSIGN 
      TOG_BERKOP:HIDDEN IN FRAME {&FRAME-NAME} = FALSE 
      TOG_KALKOP:HIDDEN = FALSE
      FILL-IN_TEXT2:HIDDEN = TRUE
      FILL-IN_TEXT3:HIDDEN = TRUE
      FILL-IN_TEXT4:HIDDEN = TRUE
      TOG_KOPPKOP:HIDDEN = TRUE.
   END.
   ELSE DO:
      
      ASSIGN
      TOG_TIDL:ROW = FILL-IN_TEXT1:ROW + 1.2.
      TOG_KOPPKOP:ROW = TOG_TIDL:ROW + 1.2.
      /*FILL-IN_TEXT2:ROW = 4*/
      FILL-IN_TEXT3:ROW = TOG_KOPPKOP:ROW + 1.2.
      FILL-IN_TEXT4:ROW = FILL-IN_TEXT3:ROW + 1.2.
      
      ASSIGN 
      TOG_BERKOP:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
      TOG_KALKOP:HIDDEN = TRUE
      FILL-IN_TEXT2:HIDDEN = TRUE
      TOG_KOPPKOP:HIDDEN = FALSE.
      
   END.
   {musarrow.i}
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY TOG_TIDL TOG_BERKOP TOG_KALKOP TOG_KOPPKOP FILL-IN_TEXT1 FILL-IN_TEXT2 
          FILL-IN_TEXT3 FILL-IN_TEXT4 
      WITH FRAME DIALOG-1.
  ENABLE TOG_TIDL TOG_BERKOP TOG_KALKOP TOG_KOPPKOP BTN_OK FILL-IN_TEXT1 
         FILL-IN_TEXT2 FILL-IN_TEXT3 FILL-IN_TEXT4 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

