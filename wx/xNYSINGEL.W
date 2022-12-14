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

/* Local Variable Definitions ---                                       */
{ALLDEF.I}



DEFINE VARIABLE dbvar AS CHARACTER NO-UNDO. 
DEFINE VARIABLE gamladb AS CHARACTER NO-UNDO. 
DEFINE VARIABLE nydb AS CHARACTER NO-UNDO. 
DEFINE VARIABLE tempvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE felkod AS INTEGER NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommando2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE arbdbb AS CHARACTER NO-UNDO.
DEFINE NEW SHARED TEMP-TABLE tabnamn NO-UNDO
   FIELD NAMN AS CHARACTER
   INDEX NAMN IS PRIMARY NAMN.
RUN PROVAG.P.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-CD FILL-IN-DBNAMN FILL-IN-DB ~
FILL-IN-START Btn_OK Btn_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-CD FILL-IN-DBNAMN FILL-IN-DB ~
FILL-IN-START 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_AVB 
     LABEL "Avbryt" 
     SIZE 15 BY 1.13
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK 
     LABEL "Ok" 
     SIZE 15 BY 1.13
     BGCOLOR 8 .

DEFINE VARIABLE FILL-IN-CD AS CHARACTER FORMAT "X(1)":U 
     VIEW-AS FILL-IN 
     SIZE 3 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-DB AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 43 BY 1 TOOLTIP "Ange s?kv?gen till din databas avsluta med \" NO-UNDO.

DEFINE VARIABLE FILL-IN-DBNAMN AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-START AS INTEGER FORMAT "9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-CD AT ROW 3.17 COL 8 COLON-ALIGNED NO-LABEL
     FILL-IN-DBNAMN AT ROW 6 COL 8 COLON-ALIGNED NO-LABEL
     FILL-IN-DB AT ROW 8.83 COL 10 NO-LABEL
     FILL-IN-START AT ROW 11.75 COL 8 COLON-ALIGNED NO-LABEL
     Btn_OK AT ROW 14.25 COL 9
     Btn_AVB AT ROW 14.25 COL 36
     "S?kv?g till din databas :" VIEW-AS TEXT
          SIZE 44 BY 1 AT ROW 7.42 COL 10
          FONT 17
     "P? vilken enhet finns din cd spelare :" VIEW-AS TEXT
          SIZE 44 BY 1 AT ROW 1.75 COL 10
          FONT 17
     "Start l?ge :" VIEW-AS TEXT
          SIZE 44 BY 1 AT ROW 10.25 COL 10
          FONT 17
     "Vad heter din databas :" VIEW-AS TEXT
          SIZE 44 BY 1 AT ROW 4.58 COL 10
          FONT 17
     SPACE(11.12) SKIP(12.41)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Uppdatera till ny Guru version"
         DEFAULT-BUTTON Btn_OK.


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

/* SETTINGS FOR FILL-IN FILL-IN-DB IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Uppdatera till ny Guru version */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_AVB Dialog-Frame
ON CHOOSE OF Btn_AVB IN FRAME Dialog-Frame /* Avbryt */
DO:
   APPLY "GO" TO BTN_AVB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_AVB Dialog-Frame
ON GO OF Btn_AVB IN FRAME Dialog-Frame /* Avbryt */
DO:
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* Ok */
DO:
   DEFINE VARIABLE i AS INTEGER.  
   REPEAT i = 1 TO NUM-ALIASES:
      DELETE ALIAS VALUE(ALIAS(i)).     
   END.     
   REPEAT i = 1 TO NUM-DBS:
      DISCONNECT VALUE(PDBNAME((i))) NO-ERROR.
   END.
   ASSIGN
   FILL-IN-START = INPUT FILL-IN-START
   FILL-IN-CD = INPUT FILL-IN-CD
   FILL-IN-DB = INPUT FILL-IN-DB
   FILL-IN-DBNAMN = INPUT FILL-IN-DBNAMN.
   IF INDEX(FILL-IN-DB,"\",LENGTH(FILL-IN-DB)) = 0 THEN DO:
      MESSAGE "S?kv?gen till databasen skall sluta med '\'!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   ASSIGN 
   gamladb = FILL-IN-DB + FILL-IN-DBNAMN
   dbvar = FILL-IN-DB + FILL-IN-DBNAMN + ".db".
   IF FILL-IN-START = 0 THEN DO:
      IF SEARCH(dbvar) = ? THEN DO: 
         MESSAGE "S?kv?gen till databasen ?r felaktig!" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
   END.
   IF FILL-IN-CD < "A" AND FILL-IN-CD > "Z" THEN DO:
      MESSAGE "Enheten till cd spelaren felaktig!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   ASSIGN 
   nydb = FILL-IN-DB + "TOMA"
   dbvar = FILL-IN-CD + ":\pro9\guru\db\" + "TOMA.db".
   IF SEARCH(dbvar) = ? THEN DO: 
      MESSAGE "S?kv?gen till den nya databasen ?r felaktig! Kontakta Elpool i Ume? AB." 
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   MESSAGE "Har du tagit backup p? din databas ?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE val1 AS LOGICAL.
   CASE val1:
      WHEN TRUE THEN DO:
      END.
      WHEN FALSE THEN DO:
         RETURN NO-APPLY.
      END.
   END CASE.
   tempvar = SESSION:TEMP-DIRECTORY.
   {muswait.i}
   RUN NYSINGELV.P (INPUT FILL-IN-START,INPUT gamladb,INPUT nydb ,INPUT tempvar,INPUT FILL-IN-CD,INPUT FILL-IN-DBNAMN,OUTPUT felkod).
   IF felkod = 0 THEN DO:
      RUN XNYSINGELDB.P (INPUT FILL-IN-DBNAMN,OUTPUT felkod).      
   END.
   IF felkod = 0 THEN DO:     
      ASSIGN
      kommando = "copy " + FILL-IN-CD + ":\pro9\guru\wtid\*.*" + " " + wtidvar + "*.*".
      OS-COMMAND VALUE(kommando).
      ASSIGN
      kommando = "copy " + FILL-IN-CD + ":\pro9\guru\wtid\bilder\*.*" + " " + wtidvar + "bilder\*.*".
      OS-COMMAND VALUE(kommando).
      MESSAGE "Allt klart!" SKIP
      "Denna Guru session kommer nu att avslutas!" SKIP
      "Starta Guru igen"
      VIEW-AS ALERT-BOX.
   END.
   {musarrow.i}
   QUIT.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON GO OF Btn_OK IN FRAME Dialog-Frame /* Ok */
DO:
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
  RUN enable_UI.
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
  DISPLAY FILL-IN-CD FILL-IN-DBNAMN FILL-IN-DB FILL-IN-START 
      WITH FRAME Dialog-Frame.
  ENABLE FILL-IN-CD FILL-IN-DBNAMN FILL-IN-DB FILL-IN-START Btn_OK Btn_AVB 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

