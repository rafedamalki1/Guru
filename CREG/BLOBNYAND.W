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
DEFINE INPUT-OUTPUT PARAMETER blobid AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER nylog AS LOGICAL NO-UNDO.

/* Local Variable Definitions ---                                       */
{ALLDEF.I}

&SCOPED-DEFINE NEW
&SCOPED-DEFINE SHARED SHARED
{BLOB.I}

DEFINE SHARED VARIABLE blobproch AS HANDLE NO-UNDO. 
DEFINE VARIABLE logresult AS LOGICAL NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE flerafiler AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN_FIL BTN_BLADDRA FILL-IN_MOD ~
FILL-IN_EXE BTN_OK BTN_AVB FILL-IN_STORLEK 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_FIL FILL-IN_MOD FILL-IN_EXE ~
FILL-IN_STORLEK 

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

DEFINE BUTTON BTN_BLADDRA 
     LABEL "Bläddra" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN_EXE AS CHARACTER FORMAT "X(256)":U 
     LABEL "Typ av händelse" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_FIL AS CHARACTER FORMAT "X(256)":U 
     LABEL "Filnamn" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_MOD AS CHARACTER FORMAT "X(256)":U 
     LABEL "Användningsormåde" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_STORLEK AS CHARACTER FORMAT "X(256)":U 
     LABEL "Storlek (kB)" 
      VIEW-AS TEXT 
     SIZE 20 BY .63 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN_FIL AT ROW 2.96 COL 19.25 COLON-ALIGNED
     BTN_BLADDRA AT ROW 2.96 COL 67.5
     FILL-IN_MOD AT ROW 5.58 COL 19.38 COLON-ALIGNED
     FILL-IN_EXE AT ROW 7.04 COL 19.38 COLON-ALIGNED
     BTN_OK AT ROW 8.29 COL 50.5
     BTN_AVB AT ROW 8.29 COL 65.5
     FILL-IN_STORLEK AT ROW 4.46 COL 19.38 COLON-ALIGNED
     "Blobinfo:" VIEW-AS TEXT
          SIZE 12 BY 1.25 AT ROW 1.33 COL 1.5
          FONT 17
     SPACE(66.49) SKIP(6.95)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Ny/ändra BLOB"
         DEFAULT-BUTTON BTN_OK CANCEL-BUTTON BTN_AVB.


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
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       FILL-IN_STORLEK:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Ny/ändra BLOB */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB Dialog-Frame
ON CHOOSE OF BTN_AVB IN FRAME Dialog-Frame /* Avbryt */
DO:
   APPLY "END-ERROR" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BLADDRA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BLADDRA Dialog-Frame
ON CHOOSE OF BTN_BLADDRA IN FRAME Dialog-Frame /* Bläddra */
DO:
/*    DEFINE VARIABLE chrfilnamn AS CHARACTER NO-UNDO. */   
/*    CURRENT-WINDOW:MOVE-TO-BOTTOM(). */
   RUN FILEMULTISEL.P ( INPUT "All Files (*.*)|*.*",
                        INPUT ?,
                        INPUT "Välj de filer som du vill lägga in i databasen.",
                        OUTPUT flerafiler,
                        OUTPUT logresult).
/*    CURRENT-WINDOW:MOVE-TO-TOP(). */
/*    SYSTEM-DIALOG GET-FILE FILL-IN_FIL                                   */
/*       TITLE "Please Select a Bitmapo or a JPEG to Add to the Database"  */
/*       FILTERS "Bitmap (*.bmp)" "*.bmp",                                 */
/*               "JPEG (*.jpg)" "*.jpg",                                   */
/*               "Word-dokument (*.doc)" "*.doc",                          */
/*               "Excel-kalkylblad (*.xls)" "*.xls",                       */
/*               "All Files (*.*)" "*.*"                                   */
/*       INITIAL-FILTER 5 MUST-EXIST RETURN-TO-START-DIR UPDATE logresult. */
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   DEFINE VARIABLE tempdir AS CHARACTER NO-UNDO.
   DEFINE VARIABLE tempfil AS CHARACTER NO-UNDO.
   IF logresult = TRUE THEN DO:                 
      tempdir = ENTRY(NUM-ENTRIES(ENTRY(1,flerafiler),"\") - 1,ENTRY(1,flerafiler),"\").
      REPEAT i = 1 TO NUM-ENTRIES(flerafiler):
         tempfil = SUBSTRING(ENTRY(i,flerafiler),R-INDEX(ENTRY(i,flerafiler),"\")).
         MESSAGE ENTRY(i,flerafiler) tempdir + tempfil VIEW-AS ALERT-BOX.
/*          SELECT-FILES:ADD-LAST(ENTRY(i,flerafiler)). */
      END.
/*       RUN filestring_UI. */
   END.
   FILL-IN_FIL:SCREEN-VALUE = flerafiler.
/*    RUN updatfillin_UI. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK Dialog-Frame
ON CHOOSE OF BTN_OK IN FRAME Dialog-Frame /* Ok */
DO:
   IF nylog = TRUE THEN DO:
      FILE-INFO:FILE-NAME = FILL-IN_FIL.
      IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
         MESSAGE "Kan inte hitta filen " FILL-IN_FIL "!" 
            "Väl ett nytt filnamn eller tryck på Avbryt." SKIP VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
      ELSE DO:
         RUN blobcheck_UI IN blobproch (INPUT FILL-IN_FIL, OUTPUT blobid).
         IF blobid NE ? THEN DO:
            MESSAGE "det finns redan en fil medd detta namn, vill du ersätta denna fil?"
               VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE logresult.
            IF logresult = TRUE THEN RETURN NO-APPLY.
         END.
         RUN blobskapa_UI IN blobproch (INPUT FILL-IN_FIL, INPUT-OUTPUT blobid, INPUT FILL-IN_MOD:SCREEN-VALUE, 
                                        INPUT FILL-IN_EXE:SCREEN-VALUE, OUTPUT felmedd).
      END.     
   END.
   ELSE DO:
      RUN blobandra_UI IN blobproch (INPUT blobid, INPUT FILL-IN_MOD:SCREEN-VALUE, INPUT FILL-IN_EXE:SCREEN-VALUE).
   END.
   APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN_FIL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN_FIL Dialog-Frame
ON LEAVE OF FILL-IN_FIL IN FRAME Dialog-Frame /* Filnamn */
DO:
   RUN updatfillin_UI.
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
   RUN enable_UI.
   {FRMSIZED.I}
   FILL-IN_STORLEK:LABEL = "Filstorlek".
   IF nylog = FALSE THEN DO:
      FIND FIRST blobinfotemp WHERE blobinfotemp.ID = blobid NO-LOCK NO-ERROR.
      ASSIGN 
      FILL-IN_FIL:SCREEN-VALUE = blobinfotemp.FILNAMN
      FILL-IN_STORLEK:SCREEN-VALUE = STRING(blobinfotemp.STORLEK) + " kB"
      FILL-IN_EXE:SCREEN-VALUE = blobinfotemp.EXEPROG
      FILL-IN_MOD:SCREEN-VALUE = blobinfotemp.MODINFO
      FILL-IN_FIL:READ-ONLY = TRUE
      BTN_BLADDRA:HIDDEN = TRUE.
   END.
   {DIA_M_SLUT.I}
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
  DISPLAY FILL-IN_FIL FILL-IN_MOD FILL-IN_EXE FILL-IN_STORLEK 
      WITH FRAME Dialog-Frame.
  ENABLE FILL-IN_FIL BTN_BLADDRA FILL-IN_MOD FILL-IN_EXE BTN_OK BTN_AVB 
         FILL-IN_STORLEK 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updatfillin_UI Dialog-Frame 
PROCEDURE updatfillin_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
   IF nylog = FALSE THEN RETURN.
   FILL-IN_FIL = INPUT FRAME {&FRAME-NAME} FILL-IN_FIL.   
   FILE-INFO:FILE-NAME = FILL-IN_FIL.
   IF FILE-INFO:FULL-PATHNAME NE ? THEN DO:
      FILL-IN_STORLEK:SCREEN-VALUE = STRING(INTEGER(FILE-INFO:FILE-SIZE / 1000)) + " kB".
   END.
   ELSE DO: 
      FILL-IN_STORLEK:SCREEN-VALUE = "".
   END.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

