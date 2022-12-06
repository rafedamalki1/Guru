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

DEFINE NEW SHARED VARIABLE dlcvar AS CHARACTER NO-UNDO.



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
DEFINE VARIABLE currenta AS CHARACTER FORMAT "x(78)" NO-UNDO.
DEFINE VARIABLE currentb AS CHARACTER FORMAT "x(78)" NO-UNDO.
DEFINE VARIABLE currentc AS CHARACTER FORMAT "x(78)" NO-UNDO.
INPUT FROM OS-DIR(".") NO-ECHO.
SET currenta currentb currentc.
INPUT CLOSE.
REPEAT:
   IF INDEX(currentb,"\.") NE 0 THEN currentb = REPLACE(currentb,"\.","\").
   ELSE LEAVE.
END.

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
FILL-IN-START FILL-IN-Tab Btn_OK Btn_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-CD FILL-IN-DBNAMN FILL-IN-DB ~
FILL-IN-START FILL-IN-Tab 

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
     SIZE 43 BY 1 TOOLTIP "Ange sökvägen till din databas avsluta med \" NO-UNDO.

DEFINE VARIABLE FILL-IN-DBNAMN AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-START AS INTEGER FORMAT "9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Tab AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-CD AT ROW 3.17 COL 8 COLON-ALIGNED NO-LABEL
     FILL-IN-DBNAMN AT ROW 6 COL 8 COLON-ALIGNED NO-LABEL
     FILL-IN-DB AT ROW 8.83 COL 10 NO-LABEL
     FILL-IN-START AT ROW 11.75 COL 8 COLON-ALIGNED NO-LABEL
     FILL-IN-Tab AT ROW 11.75 COL 31.5 COLON-ALIGNED NO-LABEL
     Btn_OK AT ROW 14.25 COL 9
     Btn_AVB AT ROW 14.25 COL 36
     "Sökväg till din databas :" VIEW-AS TEXT
          SIZE 44 BY 1 AT ROW 7.42 COL 10
          FONT 17
     "På vilken enhet finns din cd spelare :" VIEW-AS TEXT
          SIZE 44 BY 1 AT ROW 1.75 COL 10
          FONT 17
     "Start läge                    Tabell :" VIEW-AS TEXT
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
   FILL-IN-TAB = INPUT FILL-IN-TAB
   FILL-IN-START = INPUT FILL-IN-START
   FILL-IN-CD = INPUT FILL-IN-CD
   FILL-IN-DB = INPUT FILL-IN-DB
   FILL-IN-DBNAMN = INPUT FILL-IN-DBNAMN.
   IF INDEX(FILL-IN-DB,"\",LENGTH(FILL-IN-DB)) = 0 THEN DO:
      MESSAGE "Sökvägen till databasen skall sluta med '\'!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   ASSIGN 
   gamladb = FILL-IN-DB + FILL-IN-DBNAMN
   dbvar = FILL-IN-DB + FILL-IN-DBNAMN + ".db".
   IF FILL-IN-START = 0 THEN DO:
      IF SEARCH(dbvar) = ? THEN DO: 
         MESSAGE "Sökvägen till databasen är felaktig!" VIEW-AS ALERT-BOX.
         RETURN NO-APPLY.
      END.
   END.
   IF FILL-IN-CD < "A" AND FILL-IN-CD > "Z" THEN DO:
      MESSAGE "Enheten till cd spelaren felaktig!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   ASSIGN 
   /*nydb = FILL-IN-DB + "TOMA"*/
   nydb = currentb + "TOMA"
   dbvar = FILL-IN-CD + ":\pro9\guru\db\" + "TOMA.db".
   IF SEARCH(dbvar) = ? THEN DO: 
      MESSAGE "Sökvägen till den nya databasen är felaktig! Kontakta Elpool i Umeå AB." 
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.
   MESSAGE "Har du tagit backup på din databas ?"
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
   /*flyttning av db med hj av dos-kom*/
   RUN NYSINGELV.P (INPUT FILL-IN-START,INPUT gamladb,INPUT nydb ,INPUT tempvar,INPUT FILL-IN-CD,INPUT FILL-IN-DBNAMN,OUTPUT felkod).
   IF felkod = 0 THEN DO:
      /*vilka tabeller finns i den nya*/
      RUN NYSINGCON.P (INPUT FILL-IN-DBNAMN).
      FRAME {&FRAME-NAME}:TITLE = "Tabeller klara".
      IF FILL-IN-TAB NE "" THEN DO:
         FIND FIRST tabnamn WHERE tabnamn.NAMN = FILL-IN-TAB USE-INDEX NAMN NO-ERROR.
         IF NOT AVAILABLE tabnamn THEN DO:
            MESSAGE "Du har anget ett felaktigt tabellnamn!" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.   
         END.
         RUN deltab_UI (INPUT FILL-IN-DBNAMN,INPUT FILL-IN-TAB).
         FIND FIRST tabnamn USE-INDEX NAMN NO-ERROR.
         REPEAT:
            IF NOT AVAILABLE tabnamn THEN LEAVE.
            IF tabnamn.NAMN = FILL-IN-TAB THEN LEAVE.
            ELSE DELETE tabnamn.  
            FIND NEXT tabnamn USE-INDEX NAMN NO-ERROR.
         END.                      
      END.
      FOR EACH tabnamn USE-INDEX NAMN NO-LOCK:           
         FRAME {&FRAME-NAME}:TITLE = tabnamn.NAMN + " " + STRING(TIME,"HH:MM:SS").
         /*kopi data*/
         RUN NYSINGELDB.P (INPUT FILL-IN-DBNAMN,INPUT tabnamn.NAMN,OUTPUT felkod).
      END.
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
   ASSIGN
   FILL-IN-CD = "D"
   FILL-IN-DB = "C:\PRO9\GURU\DB\".
   RUN enable_UI.
   
   
   FRAME {&FRAME-NAME}:HIDDEN = FALSE. 
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE deltab_UI Dialog-Frame 
PROCEDURE deltab_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER tilldb AS CHARACTER NO-UNDO.      /*orginalnamn på db och får det nya data*/
   DEFINE INPUT PARAMETER tabell AS CHARACTER NO-UNDO.
   DEFINE VARIABLE kollkommando AS CHARACTER NO-UNDO.
   DEFINE VARIABLE tillbh AS HANDLE NO-UNDO.
   DEFINE VARIABLE qh AS HANDLE NO-UNDO.
   CREATE BUFFER tillbh FOR TABLE tabell NO-ERROR.
   kollkommando = "FOR EACH " + tilldb + "." + tabell + " NO-LOCK".  
   qh:SET-BUFFERS(tillbh).
   qh:QUERY-PREPARE(kollkommando).   
   qh:QUERY-OPEN().
   qh:GET-FIRST(NO-LOCK).  
   DO WHILE tillbh:AVAILABLE:        
      DO TRANSACTION:
         qh:GET-CURRENT(EXCLUSIVE-LOCK).  
         tillbh:BUFFER-DELETE().      
      END.
      qh:GET-NEXT(NO-LOCK).        
   END.  
   qh:QUERY-CLOSE().   

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
  DISPLAY FILL-IN-CD FILL-IN-DBNAMN FILL-IN-DB FILL-IN-START FILL-IN-Tab 
      WITH FRAME Dialog-Frame.
  ENABLE FILL-IN-CD FILL-IN-DBNAMN FILL-IN-DB FILL-IN-START FILL-IN-Tab Btn_OK 
         Btn_AVB 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

