&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          rt9              PROGRESS
          temp-db          PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-3 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 95/05/04 -  1:19 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT-OUTPUT PARAMETER vem AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{PRIARBANLTEMP.I}
{GLOBVAR2DEL1.I}
DEFINE SHARED VARIABLE anlapph AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES anlaggtemp

/* Definitions for DIALOG-BOX DIALOG-3                                  */
&Scoped-define QUERY-STRING-DIALOG-3 FOR EACH anlaggtemp NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-DIALOG-3 OPEN QUERY DIALOG-3 FOR EACH anlaggtemp NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-DIALOG-3 anlaggtemp
&Scoped-define FIRST-TABLE-IN-QUERY-DIALOG-3 anlaggtemp


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS anlaggtemp.ANLNR anlaggtemp.BENAMNING 
&Scoped-define ENABLED-TABLES anlaggtemp
&Scoped-define FIRST-ENABLED-TABLE anlaggtemp
&Scoped-Define ENABLED-OBJECTS BTN_OK BTN_AVBRYT 
&Scoped-Define DISPLAYED-FIELDS anlaggtemp.ANLNR anlaggtemp.BENAMNING 
&Scoped-define DISPLAYED-TABLES anlaggtemp
&Scoped-define FIRST-DISPLAYED-TABLE anlaggtemp


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVBRYT 
     LABEL "Avbryt":L 
     SIZE 14 BY 1.

DEFINE BUTTON BTN_OK 
     LABEL "Ok":L 
     SIZE 14 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DIALOG-3 FOR 
      anlaggtemp SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-3
     anlaggtemp.ANLNR AT ROW 1.38 COL 11 COLON-ALIGNED
          LABEL "Anl.nr" FORMAT "x(15)"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     anlaggtemp.BENAMNING AT ROW 2.71 COL 11 COLON-ALIGNED
          LABEL "Benämning"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     BTN_OK AT ROW 4 COL 10
     BTN_AVBRYT AT ROW 4 COL 25
     SPACE(1.12) SKIP(0.45)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Anläggning":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-3
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME DIALOG-3:SCROLLABLE       = FALSE
       FRAME DIALOG-3:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN anlaggtemp.ANLNR IN FRAME DIALOG-3
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN anlaggtemp.BENAMNING IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-3
/* Query rebuild information for DIALOG-BOX DIALOG-3
     _TblList          = "rt9.anlaggtemp"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-3 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-3 DIALOG-3
ON END-ERROR OF FRAME DIALOG-3 /* Anläggning */
DO:
   {BORTBRWPROC.I}
   musz = TRUE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-3 DIALOG-3
ON ENDKEY OF FRAME DIALOG-3 /* Anläggning */
DO:
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME anlaggtemp.ANLNR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL anlaggtemp.ANLNR DIALOG-3
ON LEAVE OF anlaggtemp.ANLNR IN FRAME DIALOG-3 /* Anl.nr */
DO:
   IF INPUT anlaggtemp.ANLNR = "" OR INPUT anlaggtemp.ANLNR = ? THEN DO:
      MESSAGE "Anläggning får ej vara blankt." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.  
   IF anlaggtemp.ANLNR NE INPUT anlaggtemp.ANLNR THEN DO:
      anlaggtemp.ANLNR = INPUT anlaggtemp.ANLNR.
      RUN namnkoll_UI IN anlapph (INPUT vem,INPUT anlaggtemp.ANLNR,OUTPUT TABLE felmeddtemp).                   
      FIND FIRST felmeddtemp NO-ERROR.
      IF AVAILABLE felmeddtemp THEN DO:
         MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
         DELETE felmeddtemp.
         RETURN NO-APPLY.   
      END.
   END.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_AVBRYT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVBRYT DIALOG-3
ON CHOOSE OF BTN_AVBRYT IN FRAME DIALOG-3 /* Avbryt */
DO:
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-3
ON CHOOSE OF BTN_OK IN FRAME DIALOG-3 /* Ok */
DO:
   {muswait.i}
   IF INPUT anlaggtemp.ANL = "" THEN DO:
      MESSAGE "Anläggningsnummer får ej vara blankt".
      RETURN NO-APPLY.
   END.
   ASSIGN
   anlaggtemp.BENAMNING  = INPUT anlaggtemp.BENAMNING
   anlaggtemp.ANLNR      = INPUT anlaggtemp.ANLNR.    
   RUN anlspara_UI IN anlapph (INPUT vem,INPUT TABLE anlaggtemp,OUTPUT TABLE felmeddtemp).                   
   FIND FIRST felmeddtemp NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
      DELETE felmeddtemp.
      RETURN NO-APPLY.
   END.   
   vem = anlaggtemp.ANLNR.
   {BORTBRWPROC.I}
   APPLY "GO" TO FRAME {&FRAME-NAME}.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-3 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

/* Add Trigger to equate WINDOW-CLOSE to END-ERROR                      */
ON WINDOW-CLOSE OF FRAME {&FRAME-NAME} APPLY "END-ERROR":U TO SELF.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   {DIA_M_START.I}
   RUN hamtaen_UI IN anlapph (INPUT vem,OUTPUT TABLE anlaggtemp).                   
   FIND FIRST anlaggtemp NO-ERROR.
   IF vem = ? THEN DO:
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ny".     
   END.
   ELSE DO:
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ändra-" + anlaggtemp.BENAMNING.     
   END.    
   RUN enable_UI.       
   IF vem = ? THEN ENABLE anlaggtemp.ANLNR WITH FRAME {&FRAME-NAME}. 
   ELSE DISABLE anlaggtemp.ANLNR WITH FRAME {&FRAME-NAME}. 
   {FRMSIZED.I}
   {musarrow.i}
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-3  _DEFAULT-DISABLE
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
  HIDE FRAME DIALOG-3.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-3  _DEFAULT-ENABLE
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
  IF AVAILABLE anlaggtemp THEN 
    DISPLAY anlaggtemp.ANLNR anlaggtemp.BENAMNING 
      WITH FRAME DIALOG-3.
  ENABLE anlaggtemp.ANLNR anlaggtemp.BENAMNING BTN_OK BTN_AVBRYT 
      WITH FRAME DIALOG-3.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-3}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

