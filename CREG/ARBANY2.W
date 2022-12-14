&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
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
DEFINE INPUT-OUTPUT PARAMETER vem AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}
{PRIARBANLTEMP.I}
{GLOBVAR2DEL1.I}
{ARBART.I}

DEFINE SHARED VARIABLE arbapph AS HANDLE NO-UNDO.
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
&Scoped-define INTERNAL-TABLES arbarttemp2

/* Definitions for DIALOG-BOX DIALOG-3                                  */
&Scoped-define QUERY-STRING-DIALOG-3 FOR EACH arbarttemp2 NO-LOCK
&Scoped-define OPEN-QUERY-DIALOG-3 OPEN QUERY DIALOG-3 FOR EACH arbarttemp2 NO-LOCK.
&Scoped-define TABLES-IN-QUERY-DIALOG-3 arbarttemp2
&Scoped-define FIRST-TABLE-IN-QUERY-DIALOG-3 arbarttemp2


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS arbarttemp2.ARBARTKOD ~
arbarttemp2.ARBBENAMNING arbarttemp2.AKTIV 
&Scoped-define ENABLED-TABLES arbarttemp2
&Scoped-define FIRST-ENABLED-TABLE arbarttemp2
&Scoped-Define ENABLED-OBJECTS BTN_OK BTN_AVBRYT 
&Scoped-Define DISPLAYED-FIELDS arbarttemp2.ARBARTKOD ~
arbarttemp2.ARBBENAMNING arbarttemp2.AKTIV 
&Scoped-define DISPLAYED-TABLES arbarttemp2
&Scoped-define FIRST-DISPLAYED-TABLE arbarttemp2


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
      arbarttemp2 SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-3
     arbarttemp2.ARBARTKOD AT ROW 1.75 COL 11.5 COLON-ALIGNED
          LABEL "Arbetsart"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
     arbarttemp2.ARBBENAMNING AT ROW 3.29 COL 11.5 COLON-ALIGNED
          LABEL "Ben?mning"
          VIEW-AS FILL-IN 
          SIZE 25.5 BY 1
     arbarttemp2.AKTIV AT ROW 4.83 COL 11.5 COLON-ALIGNED
          LABEL "Aktiv"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
     BTN_OK AT ROW 5.92 COL 10
     BTN_AVBRYT AT ROW 5.92 COL 25
     SPACE(0.87) SKIP(0.32)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Arbetsart":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: arbarttemp2 T "?" NO-UNDO temp-db arbarttemp2
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-3
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME DIALOG-3:SCROLLABLE       = FALSE
       FRAME DIALOG-3:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN arbarttemp2.AKTIV IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN arbarttemp2.ARBARTKOD IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN arbarttemp2.ARBBENAMNING IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-3
/* Query rebuild information for DIALOG-BOX DIALOG-3
     _TblList          = "Temp-Tables.arbarttemp2"
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-3 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-3 DIALOG-3
ON END-ERROR OF FRAME DIALOG-3 /* Arbetsart */
DO:
   musz = TRUE.
   {BORTBRWPROC.I}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-3 DIALOG-3
ON ENDKEY OF FRAME DIALOG-3 /* Arbetsart */
DO:
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME arbarttemp2.AKTIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL arbarttemp2.AKTIV DIALOG-3
ON MOUSE-SELECT-CLICK OF arbarttemp2.AKTIV IN FRAME DIALOG-3 /* Aktiv */
DO:
  IF INPUT arbarttemp2.AKTIV = TRUE THEN arbarttemp2.AKTIV = FALSE.
   IF INPUT arbarttemp2.AKTIV= FALSE THEN arbarttemp2.AKTIV = TRUE.
   DISPLAY arbarttemp2.AKTIV WITH FRAME {&FRAME-NAME}. 
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
   IF INPUT arbarttemp2.ARBARTKOD = "" /*OR INPUT arbarttemp2.ARBARTKOD = 0*/ OR INPUT arbarttemp2.ARBARTKOD = ? THEN DO:
      MESSAGE Guru.Konstanter:gartk " f?r ej vara blankt." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.  
   ASSIGN
   arbarttemp2.ARBARTKOD       = INPUT  arbarttemp2.ARBARTKOD       
   arbarttemp2.ARBBENAMNING = INPUT arbarttemp2.ARBBENAMNING.
   RUN arbspara2_UI IN arbapph (INPUT vem,INPUT TABLE arbarttemp2,OUTPUT TABLE felmeddtemp).                   
   FIND FIRST felmeddtemp NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
      DELETE felmeddtemp.
      RETURN NO-APPLY.
   END.
   vem = arbarttemp2.ARBARTKOD.
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
   RUN hamtaen2_UI IN arbapph (INPUT vem,OUTPUT TABLE arbarttemp2).                   
   FIND FIRST arbarttemp2 NO-ERROR.
   IF vem = ? THEN DO:
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ny".     
   END.
   ELSE DO TRANSACTION:
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "?ndra-" + arbarttemp2.ARBBENAMNING.    
   END.    
   arbarttemp2.ARBARTKO:LABEL = Guru.Konstanter:gartk.
   FRAME DIALOG-3:TITLE  = Guru.Konstanter:gartk.
   RUN enable_UI.       
   IF vem = ? THEN ENABLE arbarttemp2.ARBARTKOD WITH FRAME {&FRAME-NAME}. 
   ELSE DISABLE arbarttemp2.ARBARTKOD WITH FRAME {&FRAME-NAME}.
   IF Guru.Konstanter:globforetag = "Celpa" OR Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" THEN.
   ELSE arbarttemp2.AKTIV:HIDDEN = TRUE.
   {FRMSIZED.I}
   {musarrow.i}.
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
  IF AVAILABLE arbarttemp2 THEN 
    DISPLAY arbarttemp2.ARBARTKOD arbarttemp2.ARBBENAMNING arbarttemp2.AKTIV 
      WITH FRAME DIALOG-3.
  ENABLE arbarttemp2.ARBARTKOD arbarttemp2.ARBBENAMNING arbarttemp2.AKTIV 
         BTN_OK BTN_AVBRYT 
      WITH FRAME DIALOG-3.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-3}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

