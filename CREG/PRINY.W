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
DEFINE INPUT-OUTPUT PARAMETER vem AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}

{PRIARBANLTEMP.I}
{GLOBVAR2DEL1.I}
DEFINE SHARED VARIABLE priapph AS HANDLE NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES priotemp

/* Definitions for DIALOG-BOX DIALOG-3                                  */
&Scoped-define QUERY-STRING-DIALOG-3 FOR EACH priotemp SHARE-LOCK, ~
      EACH priotemp WHERE TRUE /* Join to priotemp incomplete */ SHARE-LOCK
&Scoped-define OPEN-QUERY-DIALOG-3 OPEN QUERY DIALOG-3 FOR EACH priotemp SHARE-LOCK, ~
      EACH priotemp WHERE TRUE /* Join to priotemp incomplete */ SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DIALOG-3 priotemp priotemp
&Scoped-define FIRST-TABLE-IN-QUERY-DIALOG-3 priotemp
&Scoped-define SECOND-TABLE-IN-QUERY-DIALOG-3 priotemp


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS priotemp.PKOD priotemp.P-BENAMNING ~
priotemp.AKTIV 
&Scoped-define ENABLED-TABLES priotemp
&Scoped-define FIRST-ENABLED-TABLE priotemp
&Scoped-Define ENABLED-OBJECTS BTN_OK BTN_AVBRYT 
&Scoped-Define DISPLAYED-FIELDS priotemp.PKOD priotemp.P-BENAMNING ~
priotemp.AKTIV 
&Scoped-define DISPLAYED-TABLES priotemp
&Scoped-define FIRST-DISPLAYED-TABLE priotemp


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
      priotemp, 
      priotemp SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-3
     priotemp.PKOD AT ROW 2 COL 12 COLON-ALIGNED
          LABEL "Prioritet"
          VIEW-AS FILL-IN 
          SIZE 3 BY 1
     priotemp.P-BENAMNING AT ROW 3.17 COL 12 COLON-ALIGNED
          LABEL "Benämning"
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
     priotemp.AKTIV AT ROW 4.46 COL 12.5 COLON-ALIGNED WIDGET-ID 4
          LABEL "Aktiv"
          VIEW-AS FILL-IN 
          SIZE 4.5 BY 1
     BTN_OK AT ROW 5.79 COL 11
     BTN_AVBRYT AT ROW 5.79 COL 26
     SPACE(0.74) SKIP(0.45)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Prioritet":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Temp-Tables and Buffers:
      TABLE: priotemp T "?" NO-UNDO temp-db priotemp
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-3
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME DIALOG-3:SCROLLABLE       = FALSE
       FRAME DIALOG-3:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN priotemp.AKTIV IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN priotemp.P-BENAMNING IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN priotemp.PKOD IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-3
/* Query rebuild information for DIALOG-BOX DIALOG-3
     _TblList          = "RT9.priotemp,Temp-Tables.priotemp WHERE RT9.priotemp ..."
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-3 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-3 DIALOG-3
ON END-ERROR OF FRAME DIALOG-3 /* Prioritet */
DO:
   {BORTBRWPROC.I}
   {muswait.i}
   musz = TRUE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-3 DIALOG-3
ON ENDKEY OF FRAME DIALOG-3 /* Prioritet */
DO:
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME priotemp.AKTIV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL priotemp.AKTIV DIALOG-3
ON MOUSE-SELECT-CLICK OF priotemp.AKTIV IN FRAME DIALOG-3 /* Aktiv */
DO:
  IF INPUT priotemp.AKTIV = TRUE THEN priotemp.AKTIV = FALSE.
   IF INPUT priotemp.AKTIV= FALSE THEN priotemp.AKTIV = TRUE.
   DISPLAY priotemp.AKTIV WITH FRAME {&FRAME-NAME}. 

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
   musz = FALSE.
   IF INPUT priotemp.PKOD = "" OR INPUT priotemp.PKOD = 0 OR INPUT priotemp.PKOD = ? THEN DO:
      MESSAGE  Guru.Konstanter:gpriok " får ej vara blankt." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.  
   ASSIGN
   priotemp.PKOD       = INPUT  priotemp.PKOD       
   priotemp.P-BENAMNING = INPUT priotemp.P-BENAMNING.
   RUN prispara2_UI IN priapph (INPUT vem,INPUT TABLE priotemp,OUTPUT TABLE felmeddtemp).                   
   FIND FIRST felmeddtemp NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      MESSAGE felmeddtemp.FELMEDD VIEW-AS ALERT-BOX.
      DELETE felmeddtemp.
      RETURN NO-APPLY.
   END.
   vem = priotemp.PKOD.
   {BORTBRWPROC.I}
   APPLY "GO" TO FRAME {&FRAME-NAME}.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME priotemp.PKOD
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL priotemp.PKOD DIALOG-3
ON LEAVE OF priotemp.PKOD IN FRAME DIALOG-3 /* Prioritet */
DO:
   IF INPUT priotemp.PKOD = "" OR INPUT priotemp.PKOD = 0 OR INPUT priotemp.PKOD = ? THEN DO:
      MESSAGE  Guru.Konstanter:gpriok " får ej vara blankt." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.  
   IF priotemp.PKOD NE INPUT priotemp.PKOD THEN DO:
      priotemp.PKOD = INPUT priotemp.PKOD.
      RUN namnkoll_UI IN priapph (INPUT vem,INPUT priotemp.PKOD,OUTPUT TABLE felmeddtemp).                   
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
   RUN hamtaen2_UI IN priapph (INPUT vem,OUTPUT TABLE priotemp).
   FIND FIRST priotemp NO-ERROR.
   IF vem = ? THEN DO:
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ny".     
   END.
   ELSE DO:
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ändra-" + priotemp.P-BENAMNING.          
   END.    
   priotemp.PKOD:LABEL = Guru.Konstanter:gpriok.
   RUN enable_UI.       
   IF vem = ? THEN ENABLE priotemp.PKOD WITH FRAME {&FRAME-NAME}. 
   ELSE DISABLE priotemp.PKOD WITH FRAME {&FRAME-NAME}.
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
  IF AVAILABLE priotemp THEN 
    DISPLAY priotemp.PKOD priotemp.P-BENAMNING priotemp.AKTIV 
      WITH FRAME DIALOG-3.
  ENABLE priotemp.PKOD priotemp.P-BENAMNING priotemp.AKTIV BTN_OK BTN_AVBRYT 
      WITH FRAME DIALOG-3.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-3}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

