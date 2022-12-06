&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters: ALLMENBYTSTART.W
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
&Scoped-Define ENABLED-OBJECTS Btn_OK styrtab gfalt Btn_Cancel nyfalt ~
updateorgtab 
&Scoped-Define DISPLAYED-OBJECTS styrtab gfalt nyfalt updateorgtab 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.13
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.13
     BGCOLOR 8 .

DEFINE VARIABLE gfalt AS CHARACTER FORMAT "X(256)":U 
     LABEL "fält för gamla värden" 
     VIEW-AS FILL-IN 
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE nyfalt AS CHARACTER FORMAT "X(256)":U 
     LABEL "fält för nya värden" 
     VIEW-AS FILL-IN 
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE styrtab AS CHARACTER FORMAT "X(256)":U 
     LABEL "tabell som styr" 
     VIEW-AS FILL-IN 
     SIZE 22.5 BY 1 NO-UNDO.

DEFINE VARIABLE updateorgtab AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     LABEL "om även styrtabellen ska uppdateras" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Btn_OK AT ROW 1.75 COL 81.5
     styrtab AT ROW 2 COL 21 WIDGET-ID 2 DISABLE-AUTO-ZAP 
     gfalt AT ROW 3.5 COL 36 COLON-ALIGNED WIDGET-ID 4 DISABLE-AUTO-ZAP 
     Btn_Cancel AT ROW 3.75 COL 81
     nyfalt AT ROW 6 COL 36 COLON-ALIGNED WIDGET-ID 6 DISABLE-AUTO-ZAP 
     updateorgtab AT ROW 8 COL 36 COLON-ALIGNED WIDGET-ID 10
     SPACE(48.12) SKIP(3.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "<insert dialog title>"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel WIDGET-ID 100.


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
       FRAME Dialog-Frame:SCROLLABLE       = FALSE.

/* SETTINGS FOR FILL-IN styrtab IN FRAME Dialog-Frame
   ALIGN-L                                                              */
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


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
   styrtab = INPUT styrtab.
   gfalt = INPUT  gfalt  .  /*fält för gamla värden*/
   nyfalt = INPUT  nyfalt .   /*fält för nya värden*/
   updateorgtab = INPUT updateorgtab .
   {muswait.i}
   RUN ALLMANBYT.P (INPUT gfalt, INPUT nyfalt ,INPUT styrtab ,updateorgtab).
   {musarrow.i}
   MESSAGE "KLART"
   VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME styrtab
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL styrtab Dialog-Frame
ON LEAVE OF styrtab IN FRAME Dialog-Frame /* tabell som styr */
DO:
   styrtab = INPUT styrtab.
   IF styrtab = "LEVERANTOR" THEN DO:  
       gfalt = "LEVKOD" .  /*fält för gamla värden*/
       nyfalt = "LEVORT".   /*fält för nya värden*/
       updateorgtab = TRUE.
    END.
    IF styrtab = "OMRADETAB" THEN DO:
       gfalt = "OMRADE" .  /*fält för gamla värden*/
       nyfalt = "TELJOUR".   /*fält för nya värden*/
       updateorgtab = TRUE.
    END.
    IF styrtab = "PERSONALTAB" THEN DO:
       gfalt = "PERSONALKOD" .  /*fält för gamla värden*/
       nyfalt = "ANSTNR".   /*fält för nya värden*/
       updateorgtab = TRUE.
    END.
    DISPLAY styrtab  gfalt  nyfalt updateorgtab WITH FRAME Dialog-Frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ  ?
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
  DISPLAY styrtab gfalt nyfalt updateorgtab 
      WITH FRAME Dialog-Frame.
  ENABLE Btn_OK styrtab gfalt Btn_Cancel nyfalt updateorgtab 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

