&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          temp-db          PROGRESS
*/
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
DEFINE INPUT-OUTPUT PARAMETER telvar AS CHARACTER NO-UNDO.
/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{ALLDEF.I}

&Scoped-define NEW 
&Scoped-define SHARED SHARED
{TELENAMN.I}
DEFINE SHARED VARIABLE smsproch AS HANDLE NO-UNDO. /* SMSAPP.P */
DEFINE SHARED VARIABLE andra AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.

&Scoped-define NEW 
&Scoped-define SHARED
{ANVPERS.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BRW_PERS

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES anvandartemp

/* Definitions for BROWSE BRW_PERS                                      */
&Scoped-define FIELDS-IN-QUERY-BRW_PERS anvandartemp.ANVANDARE ~
anvandartemp.AV-NAMN 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BRW_PERS 
&Scoped-define QUERY-STRING-BRW_PERS FOR EACH anvandartemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BRW_PERS OPEN QUERY BRW_PERS FOR EACH anvandartemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BRW_PERS anvandartemp
&Scoped-define FIRST-TABLE-IN-QUERY-BRW_PERS anvandartemp


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BRW_PERS}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-4 FILL-IN-NAMN FILL-IN-TEL BRW_PERS ~
FILL-IN-ENH FILL-IN-ENAMN BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-NAMN FILL-IN-TEL FILL-IN-ENH ~
FILL-IN-ENAMN 

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

DEFINE VARIABLE FILL-IN-ENAMN AS CHARACTER FORMAT "X(256)":U 
     LABEL "Namn" 
     VIEW-AS FILL-IN 
     SIZE 25 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-ENH AS CHARACTER FORMAT "X(256)":U 
     LABEL "Användare" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .83 NO-UNDO.

DEFINE VARIABLE FILL-IN-NAMN AS CHARACTER FORMAT "X(40)":U 
     LABEL "Namn" 
     VIEW-AS FILL-IN 
     SIZE 40.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TEL AS CHARACTER FORMAT "X(12)":U 
     LABEL "Telenr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 65 BY 1.21.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BRW_PERS FOR 
      anvandartemp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BRW_PERS Dialog-Frame _STRUCTURED
  QUERY BRW_PERS NO-LOCK DISPLAY
      anvandartemp.ANVANDARE COLUMN-LABEL "Användare" FORMAT "x(12)":U
      anvandartemp.AV-NAMN COLUMN-LABEL "Namn" FORMAT "x(40)":U
            WIDTH 40
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 65 BY 9.5 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-NAMN AT ROW 2.75 COL 10.13 COLON-ALIGNED
     FILL-IN-TEL AT ROW 4.17 COL 10.13 COLON-ALIGNED
     BRW_PERS AT ROW 6.21 COL 1.5
     FILL-IN-ENH AT ROW 16.13 COL 18.75 COLON-ALIGNED
     FILL-IN-ENAMN AT ROW 16.13 COL 38.25 COLON-ALIGNED
     BTN_OK AT ROW 17.5 COL 37.5
     BTN_AVB AT ROW 17.5 COL 52.5
     "SMS-användare:" VIEW-AS TEXT
          SIZE 18.13 BY .88 AT ROW 1.5 COL 1.5
          FONT 17
     "Sök på:" VIEW-AS TEXT
          SIZE 8 BY .83 AT ROW 16.13 COL 2.63
     "Ex:070 - 111 22 22 = +46701112222" VIEW-AS TEXT
          SIZE 33.5 BY 1 AT ROW 4.17 COL 27.5
     RECT-4 AT ROW 16 COL 1.5
     SPACE(0.62) SKIP(1.49)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "<insert dialog title>".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Temp-Tables and Buffers:
      TABLE: anvandartemp T "?" NO-UNDO temp-db anvandartemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BRW_PERS FILL-IN-TEL Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       BRW_PERS:ALLOW-COLUMN-SEARCHING IN FRAME Dialog-Frame = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BRW_PERS
/* Query rebuild information for BROWSE BRW_PERS
     _TblList          = "Temp-Tables.anvandartemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.anvandartemp.ANVANDARE
"anvandartemp.ANVANDARE" "Användare" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.anvandartemp.AV-NAMN
"anvandartemp.AV-NAMN" "Namn" ? "character" ? ? ? ? ? ? no ? no no "49.75" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BRW_PERS */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON END-ERROR OF FRAME Dialog-Frame /* <insert dialog title> */
DO:
  musz = TRUE.
  {BORTBRWPROC.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON ENDKEY OF FRAME Dialog-Frame /* <insert dialog title> */
DO:
  APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.
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


&Scoped-define BROWSE-NAME BRW_PERS
&Scoped-define SELF-NAME BRW_PERS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BRW_PERS Dialog-Frame
ON VALUE-CHANGED OF BRW_PERS IN FRAME Dialog-Frame
DO:
   FILL-IN-NAMN = anvandartemp.AV-NAMN.
   DISPLAY FILL-IN-NAMN WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB Dialog-Frame
ON CHOOSE OF BTN_AVB IN FRAME Dialog-Frame /* Avbryt */
DO:
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK Dialog-Frame
ON CHOOSE OF BTN_OK IN FRAME Dialog-Frame /* Ok */
DO:
   ASSIGN 
   FILL-IN-NAMN = INPUT FILL-IN-NAMN
   FILL-IN-TEL = INPUT FILL-IN-TEL.
   IF andra = FALSE THEN DO:
      FIND FIRST tele_namn WHERE tele_namn.TELNR = FILL-IN-TEL
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tele_namn THEN DO:
         CREATE tele_namn.
         ASSIGN
         tele_namn.NAMN = FILL-IN-NAMN
         tele_namn.TELNR = FILL-IN-TEL
         telvar = FILL-IN-TEL.
         MESSAGE "NYSMS:" telvar VIEW-AS ALERT-BOX.
      END.
      ELSE DO:
         MESSAGE "Detta telefonnummer finns redan upplagt."
         VIEW-AS ALERT-BOX TITLE "Meddelande".
         APPLY "ENTRY" TO FILL-IN-TEL IN FRAME {&FRAME-NAME}.
         RETURN NO-APPLY.
      END.         
   END.
   ELSE DO:
      IF telvar NE FILL-IN-TEL THEN DO:
         FIND FIRST tele_namn WHERE tele_namn.TELNR = FILL-IN-TEL NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tele_namn THEN DO:
            FIND tele_namn WHERE tele_namn.TELNR = telvar NO-LOCK NO-ERROR.
            ASSIGN
            tele_namn.NAMN = FILL-IN-NAMN
            tele_namn.TELNR = FILL-IN-TEL
            telvar = FILL-IN-TEL.
         END.   
         ELSE DO:
            MESSAGE "Detta telefonnummer finns redan upplagt."
            VIEW-AS ALERT-BOX TITLE "Meddelande".
            APPLY "ENTRY" TO FILL-IN-TEL IN FRAME {&FRAME-NAME}.
            RETURN NO-APPLY.
         END.
      END.
      ELSE DO:
         ASSIGN
         tele_namn.NAMN = FILL-IN-NAMN.
      END.   
   END.
   musz = FALSE.
   {BORTBRWPROC.I}
   APPLY "GO" TO FRAME {&FRAME-NAME}.
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
   {ALLSTARTDYN.I}
   IF andra = FALSE THEN DO:
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ny".
      FILL-IN-TEL = "+46".
   END.
   ELSE DO:
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ändra".
      FIND tele_namn WHERE tele_namn.TELNR = telvar NO-LOCK NO-ERROR.
      ASSIGN
      FILL-IN-NAMN = tele_namn.NAMN
      FILL-IN-TEL = tele_namn.TELNR.
   END.
   RUN enable_UI.       
   {FRMSIZED.I}
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
  Purpose: Changing screen-value for combo-box CMB_OMR     
  Parameters:  Input = Screen-value for CMB_FOR
  Notes:       
-------------------------------------------------------------*/       
   RUN DYNBRW.P PERSISTENT SET brwproc[1]
      (INPUT BRW_PERS:HANDLE IN FRAME {&FRAME-NAME}).
   RUN addfillin_UI IN brwproc[1]
      (INPUT FILL-IN-ENAMN:HANDLE IN FRAME {&FRAME-NAME}, INPUT "AV-NAMN").
   RUN addfillin_UI IN brwproc[1]
      (INPUT FILL-IN-ENH:HANDLE IN FRAME {&FRAME-NAME}, INPUT "ANVANDARE").
   RUN laddaanvandare_UI IN smsproch (OUTPUT TABLE anvandartemp).
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
  DISPLAY FILL-IN-NAMN FILL-IN-TEL FILL-IN-ENH FILL-IN-ENAMN 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-4 FILL-IN-NAMN FILL-IN-TEL BRW_PERS FILL-IN-ENH FILL-IN-ENAMN 
         BTN_OK BTN_AVB 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

