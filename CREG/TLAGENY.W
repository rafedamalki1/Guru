&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
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
DEFINE INPUT-OUTPUT PARAMETER idtidlag AS CHARACTER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}

{HOPALLA.I}
&Scoped-define NEW

DEFINE SHARED VARIABLE tidlagproch AS HANDLE NO-UNDO. /* Procedure handle till TIDLAGAPP.P */
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.

DEFINE SHARED TEMP-TABLE tidslagtemp NO-UNDO
   FIELD AKTIVITET1  AS CHARACTER 
   FIELD AKTIVITET2  AS CHARACTER 
   FIELD DATUM1      AS DATE       FORMAT "99/99/99" INITIAL ?
   FIELD DATUM2      AS DATE       FORMAT "99/99/99" INITIAL ?
   FIELD TIDLAGE     AS CHARACTER 
   FIELD IDTIDLAG    AS CHARACTER 
   FIELD AUTOMATISKT AS LOGICAL    FORMAT "Auto/Man" INITIAL FALSE
   INDEX IDTIDLAGE IS PRIMARY IDTIDLAG TIDLAGE
   INDEX TIDLAGE TIDLAGE IDTIDLAG.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DIALOG-3

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN_TIDLAGE FILL-IN-ARENDE FILL-IN_AKT1 ~
FILL-IN_AKT2 BTN_OK BTN_AVBRYT 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_TIDLAGE FILL-IN-ARENDE ~
FILL-IN_AKT1 FILL-IN_AKT2 

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

DEFINE VARIABLE FILL-IN-ARENDE AS LOGICAL FORMAT "Ja/Nej":U INITIAL NO 
     LABEL "Ärende" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_AKT1 AS CHARACTER FORMAT "X(15)" 
     LABEL "Aktivitet" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE FILL-IN_AKT2 AS CHARACTER FORMAT "X(15)" 
     LABEL "Aktivitet" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE FILL-IN_TIDLAGE AS CHARACTER FORMAT "X(256)" 
     LABEL "Tidläge" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-3
     FILL-IN_TIDLAGE AT ROW 1.5 COL 15 COLON-ALIGNED
     FILL-IN-ARENDE AT ROW 3 COL 15 COLON-ALIGNED WIDGET-ID 2
     FILL-IN_AKT1 AT ROW 4.5 COL 15 COLON-ALIGNED
     FILL-IN_AKT2 AT ROW 6 COL 15 COLON-ALIGNED
     BTN_OK AT ROW 7.5 COL 13.38
     BTN_AVBRYT AT ROW 7.5 COL 28.38
     SPACE(4.36) SKIP(1.03)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Tidlägen":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-3
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME DIALOG-3:SCROLLABLE       = FALSE
       FRAME DIALOG-3:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-3
/* Query rebuild information for DIALOG-BOX DIALOG-3
     _Options          = "NO-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-3 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME DIALOG-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-3 DIALOG-3
ON END-ERROR OF FRAME DIALOG-3 /* Tidlägen */
DO:
   {muswait.i}
   musz = TRUE.
   {BORTBRWPROC.I}                                                                     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL DIALOG-3 DIALOG-3
ON ENDKEY OF FRAME DIALOG-3 /* Tidlägen */
DO:
   APPLY "END-ERROR":U TO FRAME {&FRAME-NAME}.
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
   ASSIGN 
   FILL-IN-ARENDE = INPUT FILL-IN-ARENDE
   FILL-IN_TIDLAGE = INPUT FILL-IN_TIDLAGE
   FILL-IN_AKT1 = INPUT FILL-IN_AKT1
   FILL-IN_AKT2 = INPUT FILL-IN_AKT2
   musz = FALSE.
   IF FILL-IN_TIDLAGE = "" THEN DO:
      MESSAGE Guru.Konstanter:gtidlk " får ej vara blankt." VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
   END.  
   ELSE DO:
      FIND FIRST tidslagtemp WHERE tidslagtemp.IDTIDLAG = FILL-IN_TIDLAGE NO-LOCK NO-ERROR.    
      IF AVAILABLE tidslagtemp AND FILL-IN_TIDLAGE NE idtidlag THEN DO:
         MESSAGE Guru.Konstanter:gtidlk  " finns redan." VIEW-AS ALERT-BOX.                   
         RETURN NO-APPLY.
      END.
      ELSE IF idtidlag = "" THEN DO:
         CREATE tidslagtemp.
         ASSIGN
         idtidlag = FILL-IN_TIDLAGE
         tidslagtemp.IDTIDLAG = idtidlag 
         tidslagtemp.TIDLAGE = idtidlag
         tidslagtemp.AKTIVITET1 = FILL-IN_AKT1   
         tidslagtemp.AKTIVITET2 = FILL-IN_AKT2
         tidslagtemp.AUTOMATISKT = FILL-IN-ARENDE.
         RUN nytidlag_UI IN tidlagproch
            (INPUT idtidlag, INPUT TABLE tidslagtemp).         
      END.
      ELSE DO:
         FIND FIRST tidslagtemp WHERE tidslagtemp.IDTIDLAG = idtidlag NO-LOCK NO-ERROR.    
         IF AVAILABLE tidslagtemp THEN DO:
            ASSIGN
            tidslagtemp.TIDLAGE = FILL-IN_TIDLAGE 
            tidslagtemp.AKTIVITET1 = FILL-IN_AKT1   
            tidslagtemp.AKTIVITET2 = FILL-IN_AKT2
            tidslagtemp.AUTOMATISKT = FILL-IN-ARENDE.
            RUN andratidlag_UI IN tidlagproch
               (INPUT idtidlag, INPUT TABLE tidslagtemp).
         END.
      END.
   END.
   {BORTBRWPROC.I} 
   APPLY "GO" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-ARENDE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ARENDE DIALOG-3
ON LEAVE OF FILL-IN-ARENDE IN FRAME DIALOG-3 /* Ärende */
DO:
   IF FILL-IN-ARENDE:SCREEN-VALUE = "Ja" THEN FILL-IN_AKT2:LABEL = "Sortering för ärende".
   ELSE  FILL-IN_AKT2:LABEL = "Aktivitet".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-ARENDE DIALOG-3
ON MOUSE-SELECT-CLICK OF FILL-IN-ARENDE IN FRAME DIALOG-3 /* Ärende */
DO:
   IF FILL-IN-ARENDE:SCREEN-VALUE = "Nej" THEN FILL-IN-ARENDE:SCREEN-VALUE = "Ja". 
   ELSE IF FILL-IN-ARENDE:SCREEN-VALUE = "Ja" THEN FILL-IN-ARENDE:SCREEN-VALUE = "Nej".
   IF FILL-IN-ARENDE:SCREEN-VALUE = "Ja" THEN FILL-IN_AKT2:LABEL = "Sortering för ärende".
   ELSE  FILL-IN_AKT2:LABEL = "Aktivitet".
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
   
   FILL-IN_TIDLAGE:LABEL IN FRAME DIALOG-3 = Guru.Konstanter:gtidlk.
   FRAME DIALOG-3:TITLE = Guru.Konstanter:gtidlk.
   IF idtidlag NE "" THEN DO:
      FIND FIRST tidslagtemp WHERE tidslagtemp.IDTIDLAG = idtidlag NO-LOCK NO-ERROR.
      ASSIGN
      FILL-IN_TIDLAGE = tidslagtemp.TIDLAGE
      FILL-IN_AKT1 = tidslagtemp.AKTIVITET1
      FILL-IN_AKT2 = tidslagtemp.AKTIVITET2
      FILL-IN-ARENDE = tidslagtemp.AUTOMATISKT.
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ändra-" + tidslagtemp.TIDLAGE.      
   END.
   ELSE DO:
      ASSIGN FRAME {&FRAME-NAME}:TITLE = "Ny".
   END.
   RUN enable_UI. 
   /*
   IF Guru.Konstanter:hoppsekvar[16] = TRUE THEN.
   ELSE DO:
      FILL-IN-ARENDE:HIDDEN = TRUE.
   END.
   */  
    FILL-IN-ARENDE:HIDDEN = TRUE. 
   IF FILL-IN-ARENDE = TRUE THEN FILL-IN_AKT2:LABEL = "Sortering för ärende".      
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
  DISPLAY FILL-IN_TIDLAGE FILL-IN-ARENDE FILL-IN_AKT1 FILL-IN_AKT2 
      WITH FRAME DIALOG-3.
  ENABLE FILL-IN_TIDLAGE FILL-IN-ARENDE FILL-IN_AKT1 FILL-IN_AKT2 BTN_OK 
         BTN_AVBRYT 
      WITH FRAME DIALOG-3.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-3}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

