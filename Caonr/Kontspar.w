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

  Created: 95/05/16 -  7:28 am

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER aonrvar AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER delnrvar AS INTEGER NO-UNDO. 
DEFINE INPUT PARAMETER plannrvar AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER artalvar AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
{REGVAR.I}

&Scoped-define SHARED SHARED
DEFINE SHARED VARIABLE benvar AS CHARACTER NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE SEL_K1 AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "BLANK" 
     SIZE 12 BY 5.5 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE SEL_K2 AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "BLANK" 
     SIZE 12 BY 5.5 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE SEL_K3 AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "BLANK" 
     SIZE 12 BY 5.5 NO-UNDO.

DEFINE {&NEW} SHARED VARIABLE SEL_K4 AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "BLANK" 
     SIZE 12 BY 5.5 NO-UNDO.

DEFINE  {&NEW} SHARED VARIABLE SEL_K5 AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "BLANK" 
     SIZE 12 BY 5.5 NO-UNDO.

DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE aonrrec AS RECID NO-UNDO.
DEFINE VARIABLE radnrvar AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-28 FILL-IN_BEN BTN_JA BTN_NEJ 
&Scoped-Define DISPLAYED-OBJECTS sk1 sk2 sk3 sk4 sk5 FILL-IN_BEN 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_JA AUTO-GO 
     LABEL "ok":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_NEJ AUTO-END-KEY 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN_BEN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Benämning" 
     VIEW-AS FILL-IN 
     SIZE 62.25 BY 1 NO-UNDO.

DEFINE VARIABLE sk1 AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE sk2 AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE sk3 AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE sk4 AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE sk5 AS CHARACTER FORMAT "X(6)":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 43.5 BY 2
     BGCOLOR 8 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     sk1 AT ROW 4.25 COL 3.25 COLON-ALIGNED NO-LABEL
     sk2 AT ROW 4.25 COL 12.5 NO-LABEL
     sk3 AT ROW 4.25 COL 19.75 NO-LABEL
     sk4 AT ROW 4.25 COL 27 NO-LABEL
     sk5 AT ROW 4.25 COL 32.25 COLON-ALIGNED NO-LABEL
     FILL-IN_BEN AT ROW 6.5 COL 12.25 COLON-ALIGNED
     BTN_JA AT ROW 8.75 COL 47.38
     BTN_NEJ AT ROW 8.75 COL 62.38
     "Vill du spara kontosträngen för att kunna användas flera gånger?" VIEW-AS TEXT
          SIZE 67.5 BY 1.96 AT ROW 1.5 COL 1.5
          FONT 17
     RECT-28 AT ROW 3.75 COL 1.5
     SPACE(32.37) SKIP(4.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Kontosträng":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN sk1 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN sk2 IN FRAME DIALOG-1
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN sk3 IN FRAME DIALOG-1
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN sk4 IN FRAME DIALOG-1
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN sk5 IN FRAME DIALOG-1
   NO-ENABLE                                                            */
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
ON END-ERROR OF FRAME DIALOG-1 /* Kontosträng */
DO:
   RETURN.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-1 DIALOG-1
ON ENDKEY OF FRAME DIALOG-1 /* Kontosträng */
DO:
    RETURN.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_JA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_JA DIALOG-1
ON CHOOSE OF BTN_JA IN FRAME DIALOG-1 /* ok */
DO: 
   IF benvar = "" THEN DO:
      MESSAGE "Benämningen kan inte vara blank." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   IF Guru.Konstanter:appcon THEN DO:
      RUN KONTSPARAPP.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT
      (INPUT aonrvar,INPUT delnrvar,INPUT plannrvar,INPUT artalvar,
      INPUT benvar,INPUT sk1,INPUT sk2,INPUT sk3,
      INPUT sk4,INPUT sk5).    
   END.
   ELSE DO:
      RUN KONTSPARAPP.P
      (INPUT aonrvar,INPUT delnrvar,INPUT plannrvar,INPUT artalvar,
      INPUT benvar,INPUT sk1,INPUT sk2,INPUT sk3,
      INPUT sk4,INPUT sk5).    
   END.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_NEJ
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_NEJ DIALOG-1
ON CHOOSE OF BTN_NEJ IN FRAME DIALOG-1 /* Avbryt */
DO: 
   RETURN. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_BEN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_BEN DIALOG-1
ON LEAVE OF FILL-IN_BEN IN FRAME DIALOG-1 /* Benämning */
DO:
   FILL-IN_BEN = INPUT FILL-IN_BEN.
   ASSIGN benvar = FILL-IN_BEN.
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
   benvar = "".
   {DIA_M_START.I}
    
   IF SEL_K1 = "BLANK" THEN sk1 = "" .
   ELSE IF SEL_K1 = ? THEN sk1 = "" .
   ELSE sk1 = SEL_K1.
   IF SEL_K2 = "BLANK" THEN sk2 = "" .
   ELSE IF SEL_K2 = ? THEN sk2 = "" .
   ELSE sk2 = SEL_K2.
   IF SEL_K3 = "BLANK" THEN sk3 = "" .
   ELSE IF SEL_K3 = ? THEN sk3 = "" .
   ELSE sk3 = SEL_K3.
   IF SEL_K4 = "BLANK" THEN sk4 = "" .
   ELSE IF SEL_K4 = ? THEN sk4 = "" .   
   ELSE sk4 = SEL_K4.
   IF SEL_K5 = "BLANK" THEN sk5 = "" .
   ELSE IF SEL_K5 = ? THEN sk5 = "" .
   ELSE sk5 = SEL_K5.
   RUN enable_UI.       
   {FRMSIZED.I}
   IF Guru.Konstanter:globforetag = "elpa" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
      APPLY "ENTRY" TO BTN_NEJ.
   END.
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
  DISPLAY sk1 sk2 sk3 sk4 sk5 FILL-IN_BEN 
      WITH FRAME DIALOG-1.
  ENABLE RECT-28 FILL-IN_BEN BTN_JA BTN_NEJ 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

