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
/* DEFINE INPUT PARAMETER typval AS CHARACTER. */
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
/* {GLOBVAR2DEL1.I} */
DEFINE VARIABLE edith AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS EDITOR-TEXT FILL-IN-1 BTN-SAVE BTN-LOAD ~
BTN_AVB RECT-1 
&Scoped-Define DISPLAYED-OBJECTS EDITOR-TEXT FILL-IN-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN-LOAD 
     LABEL "Ladda" 
     SIZE 13.5 BY 1.13 TOOLTIP "Laddar fill utifrån ID värdet.".

DEFINE BUTTON BTN-SAVE 
     LABEL "Spara" 
     SIZE 13.5 BY 1.13 TOOLTIP "Spara undan text, ange unikt ID till vänster.".

DEFINE BUTTON BTN_AVB AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 13.5 BY 1.13
     BGCOLOR 8 .

DEFINE VARIABLE EDITOR-TEXT AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL LARGE
     SIZE 60.5 BY 17.25 NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U 
     LABEL "ID" 
     VIEW-AS FILL-IN 
     SIZE 9.5 BY 1.13 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 62.5 BY 19.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     EDITOR-TEXT AT ROW 1.5 COL 2.5 NO-LABEL
     FILL-IN-1 AT ROW 19.25 COL 4.63 COLON-ALIGNED
     BTN-SAVE AT ROW 19.25 COL 17.75
     BTN-LOAD AT ROW 19.25 COL 32.88
     BTN_AVB AT ROW 19.25 COL 48
     RECT-1 AT ROW 1.25 COL 1.5
     SPACE(0.49) SKIP(0.20)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Hjälptext"
         CANCEL-BUTTON BTN_AVB.


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
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Hjälptext */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-LOAD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-LOAD Dialog-Frame
ON CHOOSE OF BTN-LOAD IN FRAME Dialog-Frame /* Ladda */
DO:
  RUN loadtext_UI.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-SAVE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-SAVE Dialog-Frame
ON CHOOSE OF BTN-SAVE IN FRAME Dialog-Frame /* Spara */
DO:
    RUN savetext_UI.  
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
/*    {DIA_M_START.I} */
   {muswait.i}

/*    RUN loadedit_UI. */
   RUN enable_UI.
/*    {FRMSIZED.I} */
   {musarrow.i}
/*    {DIA_M_SLUT.I} */
   WAIT-FOR CHOOSE OF BTN_AVB.
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
  DISPLAY EDITOR-TEXT FILL-IN-1 
      WITH FRAME Dialog-Frame.
  ENABLE EDITOR-TEXT FILL-IN-1 BTN-SAVE BTN-LOAD BTN_AVB RECT-1 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadedit_UI Dialog-Frame 
PROCEDURE loadedit_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/           

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE loadtext_UI Dialog-Frame 
PROCEDURE loadtext_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE teststring AS CHARACTER NO-UNDO.
   DEFINE VARIABLE loadfile AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lengthstring AS INTEGER NO-UNDO.
   DEFINE VARIABLE memtext AS MEMPTR.
   DEFINE VARIABLE edithandle AS HANDLE.
   DEFINE VARIABLE editsearch AS CHARACTER.
   DEFINE VARIABLE textfound AS LOGICAL.
   DEFINE VARIABLE contcheck AS INTEGER.
   DEFINE VARIABLE starttext AS INTEGER.
   DEFINE VARIABLE sluttext AS INTEGER.

   DEFINE VARIABLE EDITOR-TEST AS CHARACTER 
      VIEW-AS EDITOR SCROLLBAR-VERTICAL LARGE 
      SIZE 1 BY 1 NO-UNDO.
   DEFINE FRAME Testframe EDITOR-TEST.

   loadfile = "c:\Mikael\mikaeltest.txt".
   FILE-INFO:FILE-NAME = loadfile.
   SET-SIZE(memtext) = FILE-INFO:FILE-SIZE.
   MESSAGE FILE-INFO:FILE-SIZE.
   edithandle = EDITOR-TEXT:HANDLE IN FRAME {&FRAME-NAME}.
   INPUT FROM VALUE(loadfile) NO-ECHO.
   IMPORT memtext.
   INPUT CLOSE.
   lengthstring = GET-SIZE(memtext).
   MESSAGE lengthstring.
   ASSIGN
   EDITOR-TEXT:READ-ONLY IN FRAME {&FRAME-NAME} = FALSE
   EDITOR-TEST:READ-ONLY IN FRAME Testframe = FALSE
   EDITOR-TEST:VISIBLE IN FRAME Testframe = FALSE
   EDITOR-TEST:SCREEN-VALUE = GET-STRING(memtext, 1, lengthstring)
   editsearch = "U10"
   contcheck = 1.
   MESSAGE EDITOR-TEST:CURSOR-OFFSET VIEW-AS ALERT-BOX.
   textfound = EDITOR-TEST:SEARCH(editsearch, 1).
   starttext = EDITOR-TEST:CURSOR-OFFSET.
   lengthstring = lengthstring - starttext.
   DO WHILE textfound = TRUE AND contcheck LE 15:
      MESSAGE EDITOR-TEST:CURSOR-OFFSET STRING(contcheck) VIEW-AS ALERT-BOX.
      textfound = EDITOR-TEST:SEARCH(editsearch, 1).
      contcheck = contcheck + 1.
   END.
   EDITOR-TEST:SCREEN-VALUE = GET-STRING(memtext, starttext, lengthstring).
   SET-SIZE(memtext) = 0.
   MESSAGE "Färdig!". 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE savetext_UI Dialog-Frame 
PROCEDURE savetext_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE savefile AS CHARACTER NO-UNDO.
   DEFINE VARIABLE memtext AS MEMPTR.
   DEFINE VARIABLE stringsize AS INTEGER.

   savefile = "c:\Mikael\mikaeltest.txt".
   EDITOR-TEXT:READ-ONLY IN FRAME {&FRAME-NAME} = FALSE.
   stringsize = LENGTH(EDITOR-TEXT:SCREEN-VALUE, "CHARACTER").
   MESSAGE stringsize VIEW-AS ALERT-BOX.
   SET-SIZE(memtext) = stringsize.
   PUT-STRING(memtext,1,stringsize) = EDITOR-TEXT:SCREEN-VALUE.
   OUTPUT TO VALUE(savefile).
   EXPORT memtext. 
   OUTPUT CLOSE.
   SET-SIZE(memtext) = 0.
   MESSAGE "File saved: " savefile. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

