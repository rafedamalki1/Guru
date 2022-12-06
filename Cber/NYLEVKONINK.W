&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
{LEVKONTAKTTEMP.I}
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR elevkontakttemp.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
&Scoped-define NEW
{GLOBVAR2DEL1.I}
&Scoped-define SHARED SHARED
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-LEVKONTAKT FILL-IN-TELE FILL-IN-FAX ~
FILL-IN-ADRESS FILL-IN-POST FILL-IN-ORT BTN_OK BTN_AVB 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-LEVKONTAKT FILL-IN-TELE ~
FILL-IN-FAX FILL-IN-ADRESS FILL-IN-POST FILL-IN-ORT 

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

DEFINE VARIABLE FILL-IN-ADRESS AS CHARACTER FORMAT "X(40)":U 
     LABEL "Adress" 
     VIEW-AS FILL-IN 
     SIZE 39.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-FAX AS CHARACTER FORMAT "X(15)":U 
     LABEL "Fax" 
     VIEW-AS FILL-IN 
     SIZE 16.38 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-LEVKONTAKT AS CHARACTER FORMAT "X(40)":U 
     LABEL "Kontaktperson" 
     VIEW-AS FILL-IN 
     SIZE 39.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-ORT AS CHARACTER FORMAT "X(30)":U 
     LABEL "Ort" 
     VIEW-AS FILL-IN 
     SIZE 30.13 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-POST AS CHARACTER FORMAT "X(6)":U 
     LABEL "Postnr" 
     VIEW-AS FILL-IN 
     SIZE 7.5 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-TELE AS CHARACTER FORMAT "X(15)":U 
     LABEL "Telefon" 
     VIEW-AS FILL-IN 
     SIZE 16.38 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FILL-IN-LEVKONTAKT AT ROW 2 COL 15.5 COLON-ALIGNED
     FILL-IN-TELE AT ROW 4 COL 15.5 COLON-ALIGNED
     FILL-IN-FAX AT ROW 6 COL 15.5 COLON-ALIGNED
     FILL-IN-ADRESS AT ROW 8 COL 15.5 COLON-ALIGNED
     FILL-IN-POST AT ROW 10 COL 15.5 COLON-ALIGNED
     FILL-IN-ORT AT ROW 10 COL 30.25 COLON-ALIGNED
     BTN_OK AT ROW 11.75 COL 33.38
     BTN_AVB AT ROW 11.75 COL 48.38
     SPACE(0.99) SKIP(0.41)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Leverantörskontakt".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON END-ERROR OF FRAME Dialog-Frame /* Leverantörskontakt */
DO:  
   musz = TRUE.
   RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON ENDKEY OF FRAME Dialog-Frame /* Leverantörskontakt */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Leverantörskontakt */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVB
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVB Dialog-Frame
ON CHOOSE OF BTN_AVB IN FRAME Dialog-Frame /* Avbryt */
DO:
   APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK Dialog-Frame
ON CHOOSE OF BTN_OK IN FRAME Dialog-Frame /* Ok */
DO:
   elevkontakttemp.LEVTEL = "".
    
   ASSIGN
   elevkontakttemp.LEVKONTAKT                 =  INPUT FRAME {&FRAME-NAME} FILL-IN-LEVKONTAKT
   elevkontakttemp.LEVADR                     =  INPUT FILL-IN-ADRESS    
   SUBSTRING(elevkontakttemp.LEVTEL,1,19)     =  REPLACE(INPUT FILL-IN-TELE," ","")      
   SUBSTRING(elevkontakttemp.LEVTEL,20)       =  REPLACE(INPUT FILL-IN-FAX," ","")       
   elevkontakttemp.LEVPNR                     =  INPUT FILL-IN-POST      
   elevkontakttemp.LEVORT                     =  INPUT FILL-IN-ORT.       
   musz = FALSE.
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
   
   {&WINDOW-NAME}:TITLE = "Kontaktperson".
   FIND FIRST elevkontakttemp NO-LOCK NO-ERROR.
   ASSIGN
   FILL-IN-LEVKONTAKT = elevkontakttemp.LEVKONTAKT
   FILL-IN-ADRESS = elevkontakttemp.LEVADR
   FILL-IN-TELE = SUBSTRING(elevkontakttemp.LEVTEL,1,19)
   FILL-IN-FAX = SUBSTRING(elevkontakttemp.LEVTEL,20)
   FILL-IN-POST = elevkontakttemp.LEVPNR
   FILL-IN-ORT = elevkontakttemp.LEVORT.
   
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
  DISPLAY FILL-IN-LEVKONTAKT FILL-IN-TELE FILL-IN-FAX FILL-IN-ADRESS 
          FILL-IN-POST FILL-IN-ORT 
      WITH FRAME Dialog-Frame.
  ENABLE FILL-IN-LEVKONTAKT FILL-IN-TELE FILL-IN-FAX FILL-IN-ADRESS 
         FILL-IN-POST FILL-IN-ORT BTN_OK BTN_AVB 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

