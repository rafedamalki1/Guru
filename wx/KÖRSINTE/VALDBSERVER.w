&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: VALDBSERVER.w

  Description:  körs inte

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

DEFINE VARIABLE wcguruwtidirstart AS CHARACTER NO-UNDO.
DEFINE VARIABLE companyname AS CHARACTER NO-UNDO.

DEFINE VARIABLE appnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE hkeyvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE vAns AS CHARACTER NO-UNDO.
{HKEYADMPER.I}  
{HKEYCURRENTUSER.I}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS wcguruwtidir guruwtidir Btn_OK Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS wcguruwtidir guruwtidir 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.13
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.13
     BGCOLOR 8 .

DEFINE VARIABLE guruwtidir AS CHARACTER FORMAT "X(256)":U 
     LABEL "WT" 
     VIEW-AS FILL-IN 
     SIZE 120 BY 1 NO-UNDO.

DEFINE VARIABLE wcguruwtidir AS CHARACTER FORMAT "X(256)":U 
     LABEL "WC" 
     VIEW-AS FILL-IN 
     SIZE 120 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     wcguruwtidir AT ROW 3 COL 3 COLON-ALIGNED WIDGET-ID 2
     guruwtidir AT ROW 5.25 COL 3 COLON-ALIGNED WIDGET-ID 4
     Btn_OK AT ROW 10.25 COL 12.5
     Btn_Cancel AT ROW 10.25 COL 40
     SPACE(70.99) SKIP(0.90)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Från wc till wtid"
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
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Från wc till wtid */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
   MESSAGE guruwtidir
   VIEW-AS ALERT-BOX
   QUESTION BUTTONS YES-NO-CANCEL UPDATE valk1 AS LOGICAL.       
   CASE valk1:
      WHEN TRUE THEN DO:
         OS-COMMAND SILENT VALUE(guruwtidir).
         MESSAGE "Klart!"
         VIEW-AS ALERT-BOX.
      END.
      WHEN FALSE THEN DO:
         APPLY "END-ERROR":U TO SELF. 
      END. 
      OTHERWISE RETURN NO-APPLY.   
   END CASE.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME guruwtidir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL guruwtidir Dialog-Frame
ON LEAVE OF guruwtidir IN FRAME Dialog-Frame /* WT */
DO:
   guruwtidir = INPUT guruwtidir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME wcguruwtidir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wcguruwtidir Dialog-Frame
ON LEAVE OF wcguruwtidir IN FRAME Dialog-Frame /* WC */
DO:
  wcguruwtidir = INPUT wcguruwtidir.
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
   DEFINE VARIABLE ejok AS LOGICAL NO-UNDO.
   ASSIGN

   companyname = "SOFTWARE\Elpool i Umeå AB\"
   appnamn = "GuruOnWeb11".

   LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
   IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      companyname = "SOFTWARE\Wow6432Node\Elpool i Umeå AB\".
      LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
   END.   
   LOAD companyname BASE-KEY hkeyvar NO-ERROR. 
   IF ERROR-STATUS:NUM-MESSAGES > 0 THEN ejok = TRUE.
   USE companyname NO-ERROR. 
   GET-KEY-VALUE SECTION appnamn KEY "ApplicationDirectory" VALUE vAns.
   UNLOAD companyname NO-ERROR.
   wcguruwtidir = vAns + "\WTID".


   ASSIGN 
   companyname = "SOFTWARE\PSC\PROGRESS\".
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN appnamn = PROVERSION.
   ELSE appnamn = "11.2".
   LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
  
   IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
      appnamn = "11.2".
      LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
      IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
         appnamn = "11.4".
         LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
      END.
      IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
         appnamn = "11.5".
         LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
      END.
      IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
         appnamn = "11.6".
         LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
      END.
      IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
         appnamn = "11.7".
         LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
      END.
      /*för att klara blandning mellan 32 bit och 64 bit.*/
      IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
         companyname = "SOFTWARE\Wow6432Node\PSC\PROGRESS\".
         appnamn = "11.2".
         LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
         IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
            appnamn = "11.4".
            LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
         END.
         IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
            appnamn = "11.5".
            LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
         END.
         IF ERROR-STATUS:NUM-MESSAGES > 0 THEN DO:
            appnamn = "11.6".
            LOAD companyname + appnamn + "\" BASE-KEY hkeyvar NO-ERROR.
         END.
      END.   
   END.
      
   IF ERROR-STATUS:NUM-MESSAGES > 0 OR ejok = TRUE THEN DO:
      MESSAGE "Funkar inte! wcguruwtidir" wcguruwtidir skip
      "companyname" companyname skip
      "appnamn" appnamn
      VIEW-AS ALERT-BOX.
      ejok = TRUE.
    
   END. 
   ELSE DO:
      UNLOAD companyname + appnamn + "\" NO-ERROR .
      /*letAr efter PROGRESS INSTALLATIONEN*/  
      hkeyvar = "HKEY_LOCAL_MACHINE".
      LOAD companyname BASE-KEY hkeyvar NO-ERROR.
      IF ERROR-STATUS:NUM-MESSAGES > 0 OR ejok = TRUE THEN ejok = TRUE.
      USE companyname NO-ERROR. 
      GET-KEY-VALUE SECTION appnamn + "\Startup" KEY "DLC" VALUE guruwtidir.
      UNLOAD companyname NO-ERROR.
   END.
   guruwtidir = REPLACE(guruwtidir,"DLC","GURU\WTID").
   guruwtidir = 'robocopy "' + wcguruwtidir + '" "' + guruwtidir + '" *.* /mir'.
   
   ejok = TRUE.
   RUN enable_UI.
   /*
   IF ejok = TRUE THEN.
   ELSE APPLY "CHOOSE" TO Btn_OK. 
   */
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
  DISPLAY wcguruwtidir guruwtidir 
      WITH FRAME Dialog-Frame.
  ENABLE wcguruwtidir guruwtidir Btn_OK Btn_Cancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

