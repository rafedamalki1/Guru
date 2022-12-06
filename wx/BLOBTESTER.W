&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}
/* För DYNBRW.P */
&Scoped-define NEW NEW
/*{EGENBVAR.I}*/
{GLOBVAR2DEL1.I}
DEFINE NEW SHARED VARIABLE appcon AS LOGICAL NO-UNDO.


DEFINE VARIABLE computername AS CHARACTER NO-UNDO.
DEFINE VARIABLE retvalue AS INTEGER NO-UNDO.
DEFINE VARIABLE blobproch AS HANDLE NO-UNDO.    
DEFINE VARIABLE logresult AS LOGICAL NO-UNDO.
DEFINE VARIABLE blobid AS INTEGER NO-UNDO.
DEFINE VARIABLE chrfilnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.

&SCOPED-DEFINE NEW NEW
&SCOPED-DEFINE SHARED SHARED
{BLOB.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BRW_IMAGE

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES blobinfotemp

/* Definitions for BROWSE BRW_IMAGE                                     */
&Scoped-define FIELDS-IN-QUERY-BRW_IMAGE blobinfotemp.FILNAMN ~
blobinfotemp.FILFORMAT blobinfotemp.STORLEK blobinfotemp.EXEPROG 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_IMAGE 
&Scoped-define QUERY-STRING-BRW_IMAGE FOR EACH blobinfotemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_IMAGE OPEN QUERY BRW_IMAGE FOR EACH blobinfotemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_IMAGE blobinfotemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_IMAGE blobinfotemp


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BRW_IMAGE}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BRW_IMAGE BTN_LADDA BTN_UPP BTN_BORT ~
BTN_EXIT IMG_BMP RECT-1 RECT-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_BORT 
     LABEL "Ta bort" 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_EXIT 
     LABEL "Exit" 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_LADDA 
     LABEL "Ladda" 
     SIZE 12 BY 1.5.

DEFINE BUTTON BTN_UPP 
     LABEL "Öppna" 
     SIZE 12 BY 1.5.

DEFINE IMAGE IMG_BMP
     FILENAME "adeicon/blank":U
     SIZE 53.5 BY 16.17.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 56.5 BY 18.08.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 60.5 BY 18.08.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_IMAGE FOR 
      blobinfotemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_IMAGE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_IMAGE C-Win _STRUCTURED
  QUERY BRW_IMAGE NO-LOCK DISPLAY
      blobinfotemp.FILNAMN FORMAT "X(30)":U WIDTH 24
      blobinfotemp.FILFORMAT FORMAT "X(3)":U
      blobinfotemp.STORLEK FORMAT ">>>>>>9":U WIDTH 13
      blobinfotemp.EXEPROG COLUMN-LABEL "Kör Prog." FORMAT "X(9)":U
            WIDTH 10.5
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS NO-COLUMN-SCROLLING SIZE 58.5 BY 14.25
         TITLE "Bitmap images".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BRW_IMAGE AT ROW 2.5 COL 1.5
     BTN_LADDA AT ROW 17.13 COL 2.25
     BTN_UPP AT ROW 17.13 COL 17.25
     BTN_BORT AT ROW 17.13 COL 32.25
     BTN_EXIT AT ROW 17.13 COL 47.13
     IMG_BMP AT ROW 2.5 COL 62.5
     RECT-1 AT ROW 1 COL 61.5
     RECT-2 AT ROW 1 COL 1
     "BLOB tester!" VIEW-AS TEXT
          SIZE 26.5 BY 1.04 AT ROW 1.38 COL 1.88
          FONT 17
     "Loaded image:" VIEW-AS TEXT
          SIZE 14.5 BY .63 AT ROW 1.79 COL 62.75
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 118.25 BY 25.54.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Temp-Tables and Buffers:
      TABLE: blobinfotemp T "?" NO-UNDO temp-db blobinfotemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "BLOB tester!"
         HEIGHT             = 18.83
         WIDTH              = 118
         MAX-HEIGHT         = 25.67
         MAX-WIDTH          = 119
         VIRTUAL-HEIGHT     = 25.67
         VIRTUAL-WIDTH      = 119
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BRW_IMAGE 1 DEFAULT-FRAME */
ASSIGN 
       BRW_IMAGE:ALLOW-COLUMN-SEARCHING IN FRAME DEFAULT-FRAME = TRUE.

ASSIGN 
       IMG_BMP:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_IMAGE
/* Query rebuild information for BROWSE BRW_IMAGE
     _TblList          = "Temp-Tables.blobinfotemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.blobinfotemp.FILNAMN
"blobinfotemp.FILNAMN" ? ? "character" ? ? ? ? ? ? no ? no no "24" yes no no "U" "" ""
     _FldNameList[2]   = Temp-Tables.blobinfotemp.FILFORMAT
     _FldNameList[3]   > Temp-Tables.blobinfotemp.STORLEK
"blobinfotemp.STORLEK" ? ? "integer" ? ? ? ? ? ? no ? no no "13" yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.blobinfotemp.EXEPROG
"blobinfotemp.EXEPROG" "Kör Prog." "X(9)" "character" ? ? ? ? ? ? no ? no no "10.5" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_IMAGE */
&ANALYZE-RESUME



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* BLOB tester! */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* BLOB tester! */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BRW_IMAGE
&Scoped-define SELF-NAME BRW_IMAGE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_IMAGE C-Win
ON VALUE-CHANGED OF BRW_IMAGE IN FRAME DEFAULT-FRAME /* Bitmap images */
DO:
/*    IF VALID-HANDLE(blobproch) THEN APPLY "CLOSE" TO blobproch. */
/*    IF VALID-HANDLE(blobproch) THEN RUN deleteproc_UI IN blobproch. */
/*       RUN deleteproc_UI IN blobproch. */
/*       DELETE PROCEDURE blobproch. */
/*    DEFINE VARIABLE temph AS HANDLE NO-UNDO.                                  */
/*    DEFINE VARIABLE nexth AS HANDLE NO-UNDO.                                  */
/*    DEFINE VARIABLE counter AS INTEGER NO-UNDO.                               */
/* /*    DEFINE VARIABLE counter2 AS INTEGER NO-UNDO. */                        */
/*    temph = SESSION:FIRST-PROCEDURE.                                          */
/*    DO WHILE VALID-HANDLE(temph):                                             */
/*       counter = counter + 1.                                                 */
/*       nexth = temph:NEXT-SIBLING.                                            */
/*       MESSAGE temph:TYPE temph:FILE-NAME temph:PERSISTENT.                   */
/* /*       IF THIS-PROCEDURE:CURRENT-WINDOW = temph:CURRENT-WINDOW THEN DO: */ */
/* /*          counter2 = counter2 + 1.                                      */ */
/* /*          DELETE PROCEDURE temph.                                       */ */
/* /*       END.                                                             */ */
/*       temph = nexth.                                                         */
/*    END.                                                                      */
/*    MESSAGE counter.                                                          */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_BORT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_BORT C-Win
ON CHOOSE OF BTN_BORT IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
   BRW_IMAGE:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   IF AVAILABLE blobinfotemp THEN DO:
      RUN blobtabort_UI IN blobproch (INPUT blobinfotemp.ID).
      RUN openbdynspec_UI IN brwproc[1].
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_EXIT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_EXIT C-Win
ON CHOOSE OF BTN_EXIT IN FRAME DEFAULT-FRAME /* Exit */
DO:
   APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_LADDA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_LADDA C-Win
ON CHOOSE OF BTN_LADDA IN FRAME DEFAULT-FRAME /* Ladda */
DO:      
   SYSTEM-DIALOG GET-FILE chrfilnamn
      TITLE "Please Select a Bitmapo or a JPEG to Add to the Database"
      FILTERS "Bitmap (*.bmp)" "*.bmp",
              "JPEG (*.jpg)" "*.jpg",
              "Word-dokument (*.doc)" "*.doc",
              "Excel-kalkylblad (*.xls)" "*.xls",
              "All Files (*.*)" "*.*"
      INITIAL-FILTER 5 MUST-EXIST RETURN-TO-START-DIR UPDATE logresult.
   IF logresult = FALSE THEN RETURN NO-APPLY.
   RUN blobcheck_UI IN blobproch (INPUT chrfilnamn, OUTPUT blobid).
   IF blobid NE ? THEN DO: 
      MESSAGE "det finns redan en fil medd detta namn, vill du ersätta denna fil?" 
         VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE logresult.
      IF logresult = TRUE THEN RETURN NO-APPLY.
   END.   
   RUN blobskapa_UI IN blobproch (INPUT chrfilnamn, INPUT-OUTPUT blobid, INPUT "BLOBTESTER", 
                                  INPUT "", OUTPUT felmedd).
   RUN openbdynspec_UI IN brwproc[1].
   FIND FIRST blobinfotemp WHERE blobinfotemp.ID = blobid NO-LOCK NO-ERROR.
   RUN selectbyrowid_UI IN brwproc[1](INPUT ROWID(blobinfotemp)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_UPP
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_UPP C-Win
ON CHOOSE OF BTN_UPP IN FRAME DEFAULT-FRAME /* Öppna */
DO:
   BRW_IMAGE:SELECT-FOCUSED-ROW() IN FRAME {&FRAME-NAME}.
   IF AVAILABLE blobinfotemp THEN DO:
      RUN bloboppnafil_UI IN blobproch (INPUT blobinfotemp.ID).      
      RUN OPENDOC.P(INPUT blobinfotemp.EXEPROG, INPUT blobinfotemp.TEMPFIL, INPUT "", INPUT FALSE).
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

 
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   IF VALID-HANDLE(blobproch) THEN RUN deleteproc_UI IN blobproch.
   RUN disable_UI.
END.
   

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   RUN DYNBRW.P PERSISTENT SET brwproc[1] (INPUT BRW_IMAGE:HANDLE IN FRAME {&FRAME-NAME}).
/*    RUN DYNBLOB.P PERSISTENT SET blobproch (INPUT Guru.Konstanter:appcon, INPUT Guru.Konstanter:apphand). */
/*    RUN DYNBLOB.P PERSISTENT SET blobproch (INPUT Guru.Konstanter:appcon). */
   IF Guru.Konstanter:appcon = TRUE THEN DO:
      RUN FINNSTABELL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT "BLOBINFO", OUTPUT logresult).
   END.
   ELSE DO:
      RUN FINNSTABELL.P (INPUT "BLOBINFO", OUTPUT logresult).
   END.
/*    RUN blobkontroll_UI IN blobproch (OUTPUT logresult). */
   RUN enable_UI.
   IF logresult = TRUE THEN DO:
      RUN DYNBLOB.P PERSISTENT SET blobproch.
      RUN blobladda_UI IN blobproch (INPUT "").
      RUN openbdynspec_UI IN brwproc[1].
   END.
   ELSE DO: 
      APPLY "CLOSE" TO THIS-PROCEDURE.
      LEAVE MAIN-BLOCK.
   END.
   IF NOT THIS-PROCEDURE:PERSISTENT THEN
   WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  ENABLE BRW_IMAGE BTN_LADDA BTN_UPP BTN_BORT BTN_EXIT IMG_BMP RECT-1 RECT-2 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

