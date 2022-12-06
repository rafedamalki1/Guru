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
/*          This .W file was created with the Progress AppBulder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER avtid AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{GLOBVAR2DEL1.I}
DEFINE SHARED VARIABLE avtregproch AS HANDLE NO-UNDO. /* AVTREGAPP.P */
/* DEFINE SHARED VARIABLE brec AS RECID NO-UNDO. */
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE OKvald AS LOGICAL INITIAL TRUE.
DEFINE VARIABLE outanvanv AS CHARACTER NO-UNDO.
DEFINE VARIABLE outdatornamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE datornamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE fildir AS CHARACTER NO-UNDO.
&SCOPED-DEFINE NEW
&SCOPED-DEFINE SHARED SHARED
{AVTALTEMP.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-AVTAL TOG_TILL BTN_FIL FILL-IN-AKTIV ~
ED_MEDD BTN_OK BTN_AVB FILL-IN-FILNAMN 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-AVTAL TOG_TILL FILL-IN-AKTIV ~
ED_MEDD FILL-IN-FILNAMN 

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

DEFINE BUTTON BTN_FIL 
     LABEL "Välj fil" 
     SIZE 12 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1
     BGCOLOR 8 .

DEFINE VARIABLE ED_MEDD AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 55 BY 5.38
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FILL-IN-AKTIV AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "Aktiv" 
     VIEW-AS FILL-IN 
     SIZE 4.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-AVTAL AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FILNAMN AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 55 BY 1
     FONT 19 NO-UNDO.

DEFINE VARIABLE TOG_TILL AS LOGICAL INITIAL no 
     LABEL "Dokument" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.13 BY .88 TOOLTIP "Om det är ett dokument kryssa denna" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-AVTAL AT ROW 2.63 COL 1.5 NO-LABEL
     TOG_TILL AT ROW 3.75 COL 1.5
     BTN_FIL AT ROW 5.08 COL 1.5
     FILL-IN-AKTIV AT ROW 9.17 COL 7.88 COLON-ALIGNED
     ED_MEDD AT ROW 11.63 COL 1.5 NO-LABEL
     BTN_OK AT ROW 17.29 COL 27.5
     BTN_AVB AT ROW 17.29 COL 42.5
     FILL-IN-FILNAMN AT ROW 7.25 COL 2.88 NO-LABEL
     "Anmärkning:" VIEW-AS TEXT
          SIZE 12 BY .67 AT ROW 10.54 COL 2.88
     "Filnamn:" VIEW-AS TEXT
          SIZE 12.5 BY .67 AT ROW 6.46 COL 2.88
     "Tillstånd/Avtal/Dokument:" VIEW-AS TEXT
          SIZE 27.5 BY 1.25 AT ROW 1.25 COL 1.5
          FONT 17
     SPACE(29.24) SKIP(16.03)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Tillstånd/Avtal/Dokument"
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
       ED_MEDD:RETURN-INSERTED IN FRAME Dialog-Frame  = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-AVTAL IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FILL-IN-FILNAMN IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON END-ERROR OF FRAME Dialog-Frame /* Tillstånd/Avtal/Dokument */
DO:
   {muswait.i}
   musz = TRUE.
   {BORTBRWPROC.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Tillstånd/Avtal/Dokument */
DO:
   APPLY "END-ERROR":U TO FRAME Dialog-Frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB Dialog-Frame
ON CHOOSE OF BTN_AVB IN FRAME Dialog-Frame /* Avbryt */
DO:
   APPLY "END-ERROR":U TO FRAME Dialog-Frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_FIL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_FIL Dialog-Frame
ON CHOOSE OF BTN_FIL IN FRAME Dialog-Frame /* Välj fil */
DO:
   RUN INLOAPI.P (OUTPUT outanvanv, OUTPUT outdatornamn).

   datornamn = TRIM(outdatornamn).
   IF Guru.Konstanter:globforetag = "GRAN" THEN DO:   
      SYSTEM-DIALOG GET-FILE filnamn
      TITLE          "Välj den fil som Ni vill koppla"
      FILTERS        "All Files (*.*)"  "*.*"   
      INITIAL-DIR    "\\SRV00486\APPL\guru\mallar\"
      MUST-EXIST         
      USE-FILENAME
      UPDATE OKvald.
      /*
      INITIAL-DIR    "\\Dokugran\dokument\Nät (N)\guru\mallar\"
      */
      
   END.
   ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
      /*
      SYSTEM-DIALOG GET-FILE filnamn
      TITLE          "Välj den fil som Ni vill koppla"
      FILTERS        "All Files (*.*)"  "*.*"   
      INITIAL-DIR    "\\Prosit.energi-miljo.se\Gemensam\GURU\mallar\"
      MUST-EXIST         
      USE-FILENAME
      UPDATE OKvald.
      */
      SYSTEM-DIALOG GET-FILE filnamn
      TITLE          "Välj den fil som Ni vill koppla"
      FILTERS        "All Files (*.*)"  "*.*"   
      INITIAL-DIR    "\\goliat\GURU\mallar\"
      MUST-EXIST         
      USE-FILENAME
      UPDATE OKvald.
   END.
   ELSE IF Guru.Konstanter:globforetag = "ELPA" THEN DO:
      SYSTEM-DIALOG GET-FILE filnamn
      TITLE          "Välj den fil som Ni vill koppla"
      FILTERS        "All Files (*.*)"  "*.*"   
      INITIAL-DIR    "\\server05\D\dokument\mallar\"
      MUST-EXIST         
      USE-FILENAME
      UPDATE OKvald.
   END.
   ELSE IF Guru.Konstanter:globforetag = "PINN" THEN DO:
      SYSTEM-DIALOG GET-FILE filnamn
      TITLE          "Välj den fil som Ni vill koppla"
      FILTERS        "All Files (*.*)"  "*.*"   
      INITIAL-DIR    SESSION:TEMP-DIR
      MUST-EXIST         
      USE-FILENAME
      UPDATE OKvald.
   END.
   ELSE IF Guru.Konstanter:globforetag = "LULE" THEN DO:
      SYSTEM-DIALOG GET-FILE filnamn
      TITLE          "Välj den fil som Ni vill koppla"
      FILTERS        "All Files (*.*)"  "*.*"   
      INITIAL-DIR    "\\" + Guru.Konstanter:InternNr(STRING(195)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(196)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(19)) + CHR(46) +  Guru.Konstanter:InternNr(STRING(21)) + "\DOKUMENT\MALLAR\"
      MUST-EXIST         
      USE-FILENAME
      UPDATE OKvald.
   END.
   ELSE IF Guru.Konstanter:globforetag = "TRAS" THEN DO:   
      SYSTEM-DIALOG GET-FILE filnamn
      TITLE          "Välj den fil som Ni vill koppla"
      FILTERS        "All Files (*.*)"  "*.*"   
      INITIAL-DIR    "C:\PRO9\GURU\DOKUMENT\MALLAR\"      
      MUST-EXIST         
      USE-FILENAME
      UPDATE OKvald.      
   END.
   ELSE DO:
      fildir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
      {SESSIONTEMPDIR.I}
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
      SYSTEM-DIALOG GET-FILE filnamn
      TITLE          "Välj den fil som Ni vill koppla"
      FILTERS        "All Files (*.*)"  "*.*"   
      INITIAL-DIR    fildir
      MUST-EXIST         
      USE-FILENAME
      UPDATE OKvald.
   END.
   IF OKvald = TRUE THEN DO:
      ASSIGN
      FILL-IN-FILNAMN = filnamn.
      DISPLAY FILL-IN-FILNAMN WITH FRAME {&FRAME-NAME}.      
   END.
   ELSE DO:
      ASSIGN
      FILL-IN-FILNAMN = "".
      DISPLAY FILL-IN-FILNAMN WITH FRAME {&FRAME-NAME}.      
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK Dialog-Frame
ON CHOOSE OF BTN_OK IN FRAME Dialog-Frame /* Ok */
DO:
   ASSIGN
   FILL-IN-AVTAL = INPUT FILL-IN-AVTAL
   TOG_TILL = INPUT TOG_TILL
   FILL-IN-FILNAMN = INPUT FILL-IN-FILNAMN
   FILL-IN-AKTIV = FILL-IN-AKTIV
   ED_MEDD = INPUT ED_MEDD.
   IF FILL-IN-AVTAL = "" THEN DO:
      MESSAGE "Tillstånd/Avtal/Dokument kan ej vara blank."
      VIEW-AS ALERT-BOX TITLE "Meddelande".
      APPLY "ENTRY" TO FILL-IN-AVTAL IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
   END.
   IF FILL-IN-FILNAMN = "" THEN DO:      
      MESSAGE "Ni har inte valt någon fil. Vill ni fortsätta?" VIEW-AS ALERT-BOX
      QUESTION BUTTONS YES-NO TITLE "Fil?" UPDATE svar AS LOGICAL.         
      IF NOT svar THEN DO:                                         
         RETURN NO-APPLY.
      END.      
   END.
   IF avtid = ? THEN DO:
      RUN skapaavtal_UI IN avtregproch (OUTPUT avtid).
      CREATE avtaltemp.
      ASSIGN
      avtaltemp.AVTALID = avtid
      avtaltemp.ORDNING = avtid.
   END.
   ELSE FIND FIRST avtaltemp WHERE avtaltemp.AVTALID = avtid NO-LOCK.
   ASSIGN
   avtaltemp.AVTAL = ""
   SUBSTRING(avtaltemp.AVTAL,1,30) = FILL-IN-AVTAL
   avtaltemp.FILNAMN = FILL-IN-FILNAMN
   avtaltemp.ANMARK = ED_MEDD
   avtaltemp.LANK = TOG_TILL.
   IF FILL-IN-AKTIV = FALSE THEN ASSIGN SUBSTRING(avtaltemp.AVTAL,35,7) = "INAKTIV".      
   RUN sparaavtal_UI IN avtregproch (INPUT avtid, INPUT TABLE avtaltemp).
   {BORTBRWPROC.I}
   APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-AKTIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-AKTIV Dialog-Frame
ON MOUSE-SELECT-CLICK OF FILL-IN-AKTIV IN FRAME Dialog-Frame /* Aktiv */
DO:
   IF INPUT FILL-IN-AKTIV = TRUE THEN FILL-IN-AKTIV = FALSE.
   IF INPUT FILL-IN-AKTIV = FALSE THEN FILL-IN-AKTIV = TRUE.
   DISPLAY FILL-IN-AKTIV WITH FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TOG_TILL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TOG_TILL Dialog-Frame
ON VALUE-CHANGED OF TOG_TILL IN FRAME Dialog-Frame /* Dokument */
DO:
   TOG_TILL = INPUT TOG_TILL.
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
   IF avtid NE ? THEN DO TRANSACTION:
      FIND FIRST avtaltemp WHERE avtaltemp.AVTALID = avtid NO-LOCK NO-ERROR.
      IF AVAILABLE avtaltemp THEN DO:
         ASSIGN 
         FRAME {&FRAME-NAME}:TITLE = "Ändring av tillstånd/avtal " + SUBSTRING(avtaltemp.AVTAL,1,30)
         FILL-IN-AVTAL = SUBSTRING(avtaltemp.AVTAL,1,30)
         FILL-IN-FILNAMN = avtaltemp.FILNAMN
         ED_MEDD = avtaltemp.ANMARK
         TOG_TILL = avtaltemp.LANK.
         IF SUBSTRING(avtaltemp.AVTAL,35,7) = "INAKTIV" THEN ASSIGN FILL-IN-AKTIV = FALSE.
         ELSE ASSIGN FILL-IN-AKTIV = TRUE.                               
      END.
   END.
   ELSE ASSIGN FRAME {&FRAME-NAME}:TITLE = "Nytt tillstånd/avtal". 
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
  DISPLAY FILL-IN-AVTAL TOG_TILL FILL-IN-AKTIV ED_MEDD FILL-IN-FILNAMN 
      WITH FRAME Dialog-Frame.
  ENABLE FILL-IN-AVTAL TOG_TILL BTN_FIL FILL-IN-AKTIV ED_MEDD BTN_OK BTN_AVB 
         FILL-IN-FILNAMN 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

