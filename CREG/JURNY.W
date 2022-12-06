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

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER vem AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{AVDELNINGTEMP.I}
&Scoped-define NEW 
{OMRALLT.I}
{GLOBVAR2DEL1.I}
DEFINE SHARED VARIABLE orgapph AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES jurperstemp

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame jurperstemp.VIJUDID ~
jurperstemp.NAMN jurperstemp.GATUADR jurperstemp.POSTNR jurperstemp.POSTADR ~
jurperstemp.TELVXL jurperstemp.TELJOUR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame jurperstemp.VIJUDID ~
jurperstemp.NAMN jurperstemp.GATUADR jurperstemp.POSTNR jurperstemp.POSTADR ~
jurperstemp.TELVXL jurperstemp.TELJOUR 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame jurperstemp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame jurperstemp
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH jurperstemp SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH jurperstemp SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame jurperstemp
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame jurperstemp


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS jurperstemp.VIJUDID jurperstemp.NAMN ~
jurperstemp.GATUADR jurperstemp.POSTNR jurperstemp.POSTADR ~
jurperstemp.TELVXL jurperstemp.TELJOUR 
&Scoped-define ENABLED-TABLES jurperstemp
&Scoped-define FIRST-ENABLED-TABLE jurperstemp
&Scoped-Define ENABLED-OBJECTS BTN_Ok BTN_AVB 
&Scoped-Define DISPLAYED-FIELDS jurperstemp.VIJUDID jurperstemp.NAMN ~
jurperstemp.GATUADR jurperstemp.POSTNR jurperstemp.POSTADR ~
jurperstemp.TELVXL jurperstemp.TELJOUR 
&Scoped-define DISPLAYED-TABLES jurperstemp
&Scoped-define FIRST-DISPLAYED-TABLE jurperstemp


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVB 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_Ok 
     LABEL "Ok":L 
     SIZE 14 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      jurperstemp SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     jurperstemp.VIJUDID AT ROW 2 COL 13.63 COLON-ALIGNED
          LABEL "Vijud"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     jurperstemp.NAMN AT ROW 2 COL 35.75 COLON-ALIGNED
          LABEL "Namn" FORMAT "X(30)"
          VIEW-AS FILL-IN 
          SIZE 27.38 BY 1
     jurperstemp.GATUADR AT ROW 3.5 COL 13.63 COLON-ALIGNED
          LABEL "Adress"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     jurperstemp.POSTNR AT ROW 5 COL 13.63 COLON-ALIGNED
          LABEL "Postnr"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     jurperstemp.POSTADR AT ROW 5 COL 35.75 COLON-ALIGNED
          LABEL "Postadress"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     jurperstemp.TELVXL AT ROW 6.5 COL 13.63 COLON-ALIGNED
          LABEL "Telefonr."
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     jurperstemp.TELJOUR AT ROW 6.5 COL 35.75 COLON-ALIGNED
          LABEL "Journr"
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
     BTN_Ok AT ROW 8.04 COL 36.13
     BTN_AVB AT ROW 8.04 COL 51.13
     SPACE(1.36) SKIP(0.24)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "<insert dialog title>".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: jurperstemp T "?" NO-UNDO temp-db jurperstemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN jurperstemp.GATUADR IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN jurperstemp.NAMN IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN jurperstemp.POSTADR IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN jurperstemp.POSTNR IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN jurperstemp.TELJOUR IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN jurperstemp.TELVXL IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN jurperstemp.VIJUDID IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "temp-db.jurperstemp"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON END-ERROR OF FRAME Dialog-Frame /* <insert dialog title> */
DO:
   {muswait.i}
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
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.
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


&Scoped-define SELF-NAME BTN_Ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_Ok Dialog-Frame
ON CHOOSE OF BTN_Ok IN FRAME Dialog-Frame /* Ok */
DO:
   {muswait.i}
   ASSIGN
   jurperstemp.VIJUDID = INPUT jurperstemp.VIJUDID   
   jurperstemp.NAMN    = INPUT jurperstemp.NAMN   
   jurperstemp.GATUADR = INPUT jurperstemp.GATUADR
   jurperstemp.POSTNR  = INPUT jurperstemp.POSTNR 
   jurperstemp.POSTADR = INPUT jurperstemp.POSTADR
   jurperstemp.TELVXL  = INPUT jurperstemp.TELVXL 
   jurperstemp.TELJOUR = INPUT jurperstemp.TELJOUR.
   IF jurperstemp.VIJUDID = "" THEN DO:
      MESSAGE Guru.Konstanter:gjuk "kan inte vara blankt!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.   
   FIND FIRST juromravd WHERE juromravd.TYP = "JUR" AND juromravd.ID = jurperstemp.VIJUDID AND
   juromravd.SID NE jurperstemp.JUDID
   NO-LOCK NO-ERROR.
   IF AVAILABLE juromravd THEN DO:
      MESSAGE Guru.Konstanter:gjuk "finns redan upplagt!" VIEW-AS ALERT-BOX.      
      RETURN NO-APPLY.
   END.   
   vem = jurperstemp.JUDID.
   RUN sparjur_UI IN orgapph (INPUT TABLE jurperstemp,OUTPUT TABLE felmeddtemp).
   APPLY "GO" TO FRAME {&FRAME-NAME}.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME jurperstemp.VIJUDID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL jurperstemp.VIJUDID Dialog-Frame
ON LEAVE OF jurperstemp.VIJUDID IN FRAME Dialog-Frame /* Vijud */
DO:
   jurperstemp.VIJUDID = INPUT jurperstemp.VIJUDID.
   IF jurperstemp.VIJUDID = ""THEN DO:
      MESSAGE Guru.Konstanter:gjuk "kan inte vara blankt!" VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.   
   FIND FIRST juromravd WHERE juromravd.TYP = "JUR" AND juromravd.ID = jurperstemp.VIJUDID AND
   juromravd.SID NE jurperstemp.JUDID
   NO-LOCK NO-ERROR.
   IF AVAILABLE juromravd THEN DO:
      MESSAGE Guru.Konstanter:gjuk "finns redan upplagt!" VIEW-AS ALERT-BOX.      
      RETURN NO-APPLY.
   END.   
   IF vem = ? THEN DO:
      jurperstemp.JUDID = jurperstemp.VIJUDID. 
   END. 
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
   RUN jurhmten_UI IN orgapph (INPUT vem,OUTPUT TABLE jurperstemp).
   FIND FIRST jurperstemp NO-ERROR.
   jurperstemp.VIJUDID:LABEL = Guru.Konstanter:gjuk.
   IF vem = ? THEN DO:
      ASSIGN 
      FRAME {&FRAME-NAME}:TITLE = "Ny " + Guru.Konstanter:gjul. 
      
   END.
   ELSE DO TRANSACTION:
      ASSIGN 
      FRAME {&FRAME-NAME}:TITLE = "Ändra-" + jurperstemp.NAMN.      
   END.
   RUN enable_UI.       
   {FRMSIZED.I}
   {musarrow.i}
   {DIA_M_SLUT.I}
   ASSIGN FRAME {&FRAME-NAME}:TITLE = Guru.Konstanter:gjul.
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
  IF AVAILABLE jurperstemp THEN 
    DISPLAY jurperstemp.VIJUDID jurperstemp.NAMN jurperstemp.GATUADR 
          jurperstemp.POSTNR jurperstemp.POSTADR jurperstemp.TELVXL 
          jurperstemp.TELJOUR 
      WITH FRAME Dialog-Frame.
  ENABLE jurperstemp.VIJUDID jurperstemp.NAMN jurperstemp.GATUADR 
         jurperstemp.POSTNR jurperstemp.POSTADR jurperstemp.TELVXL 
         jurperstemp.TELJOUR BTN_Ok BTN_AVB 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

