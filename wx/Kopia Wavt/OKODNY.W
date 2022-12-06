&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          rt8              PROGRESS
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
DEFINE INPUT PARAMETER BESTRECO AS RECID NO-UNDO.
/* Local Variable Definitions ---                                       */
DEFINE SHARED VARIABLE brec AS RECID NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES OVERKOD

/* Definitions for DIALOG-BOX DIALOG-3                                  */
&Scoped-define FIELDS-IN-QUERY-DIALOG-3 OVERKOD.OVERTIDTILL OVERKOD.KOD ~
OVERKOD.VILART OVERKOD.LONKODTEXT OVERKOD.OVERTIDUTTAG OVERKOD.ERSATTNING ~
OVERKOD.ENKEL OVERKOD.ENHET OVERKOD.MULTIP 
&Scoped-define ENABLED-FIELDS-IN-QUERY-DIALOG-3 OVERKOD.OVERTIDTILL ~
OVERKOD.KOD OVERKOD.VILART OVERKOD.LONKODTEXT OVERKOD.OVERTIDUTTAG ~
OVERKOD.ERSATTNING OVERKOD.ENKEL OVERKOD.ENHET OVERKOD.MULTIP 
&Scoped-define ENABLED-TABLES-IN-QUERY-DIALOG-3 OVERKOD
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-DIALOG-3 OVERKOD

&Scoped-define FIELD-PAIRS-IN-QUERY-DIALOG-3~
 ~{&FP1}OVERTIDTILL ~{&FP2}OVERTIDTILL ~{&FP3}~
 ~{&FP1}KOD ~{&FP2}KOD ~{&FP3}~
 ~{&FP1}VILART ~{&FP2}VILART ~{&FP3}~
 ~{&FP1}LONKODTEXT ~{&FP2}LONKODTEXT ~{&FP3}~
 ~{&FP1}OVERTIDUTTAG ~{&FP2}OVERTIDUTTAG ~{&FP3}~
 ~{&FP1}ERSATTNING ~{&FP2}ERSATTNING ~{&FP3}~
 ~{&FP1}ENKEL ~{&FP2}ENKEL ~{&FP3}~
 ~{&FP1}ENHET ~{&FP2}ENHET ~{&FP3}~
 ~{&FP1}MULTIP ~{&FP2}MULTIP ~{&FP3}
&Scoped-define OPEN-QUERY-DIALOG-3 OPEN QUERY DIALOG-3 FOR EACH OVERKOD SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DIALOG-3 OVERKOD
&Scoped-define FIRST-TABLE-IN-QUERY-DIALOG-3 OVERKOD


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS OVERKOD.OVERTIDTILL OVERKOD.KOD ~
OVERKOD.VILART OVERKOD.LONKODTEXT OVERKOD.OVERTIDUTTAG OVERKOD.ERSATTNING ~
OVERKOD.ENKEL OVERKOD.ENHET OVERKOD.MULTIP 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}OVERTIDTILL ~{&FP2}OVERTIDTILL ~{&FP3}~
 ~{&FP1}KOD ~{&FP2}KOD ~{&FP3}~
 ~{&FP1}VILART ~{&FP2}VILART ~{&FP3}~
 ~{&FP1}LONKODTEXT ~{&FP2}LONKODTEXT ~{&FP3}~
 ~{&FP1}OVERTIDUTTAG ~{&FP2}OVERTIDUTTAG ~{&FP3}~
 ~{&FP1}ERSATTNING ~{&FP2}ERSATTNING ~{&FP3}~
 ~{&FP1}ENKEL ~{&FP2}ENKEL ~{&FP3}~
 ~{&FP1}ENHET ~{&FP2}ENHET ~{&FP3}~
 ~{&FP1}MULTIP ~{&FP2}MULTIP ~{&FP3}
&Scoped-define ENABLED-TABLES OVERKOD
&Scoped-define FIRST-ENABLED-TABLE OVERKOD
&Scoped-Define ENABLED-OBJECTS RECT-12 RECT-11 BTN_OK BTN_AVBRYT 
&Scoped-Define DISPLAYED-FIELDS OVERKOD.OVERTIDTILL OVERKOD.KOD ~
OVERKOD.VILART OVERKOD.LONKODTEXT OVERKOD.OVERTIDUTTAG OVERKOD.ERSATTNING ~
OVERKOD.ENKEL OVERKOD.ENHET OVERKOD.MULTIP 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVBRYT AUTO-END-KEY 
     LABEL "AVBRYT":L 
     SIZE 11 BY 1.18.

DEFINE BUTTON BTN_OK 
     LABEL "OK":L 
     SIZE 10.5 BY 1.18.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 43.5 BY 11.45
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 43.5 BY 3
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DIALOG-3 FOR 
      OVERKOD SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-3
     OVERKOD.OVERTIDTILL AT ROW 2.27 COL 19.13 COLON-ALIGNED
          LABEL "Overtidtill"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
          BGCOLOR 8 
     OVERKOD.KOD AT ROW 3.36 COL 19.13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3 BY 1
          BGCOLOR 8 
     OVERKOD.VILART AT ROW 4.45 COL 19.13 COLON-ALIGNED
          LABEL "Visad löneart"
          VIEW-AS FILL-IN 
          SIZE 6 BY .86
          BGCOLOR 8 
     OVERKOD.LONKODTEXT AT ROW 5.36 COL 19.13 COLON-ALIGNED
          LABEL "Text" FORMAT "x(45)"
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
          BGCOLOR 8 
     OVERKOD.OVERTIDUTTAG AT ROW 6.36 COL 19.13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 2 BY 1
          BGCOLOR 8 
     OVERKOD.ERSATTNING AT ROW 7.36 COL 19.13 COLON-ALIGNED
          LABEL "Ersättning kr"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 8 
     OVERKOD.ENKEL AT ROW 8.36 COL 19.13 COLON-ALIGNED
          LABEL "Enke/Kval"
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
          BGCOLOR 8 
     OVERKOD.ENHET AT ROW 9.36 COL 19.13 COLON-ALIGNED
          LABEL "Enhet"
          VIEW-AS FILL-IN 
          SIZE 3 BY 1
          BGCOLOR 8 
     OVERKOD.MULTIP AT ROW 10.59 COL 19.13 COLON-ALIGNED
          LABEL "Multip" FORMAT "->>>>9.999"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 8 
     BTN_OK AT ROW 13.5 COL 10.5
     BTN_AVBRYT AT ROW 13.5 COL 25
     RECT-12 AT ROW 12.5 COL 2
     RECT-11 AT ROW 1.05 COL 2
     SPACE(1.62) SKIP(3.63)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Overkod":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-3
                                                                        */
ASSIGN 
       FRAME DIALOG-3:SCROLLABLE       = FALSE.

/* SETTINGS FOR FILL-IN OVERKOD.ENHET IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN OVERKOD.ENKEL IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN OVERKOD.ERSATTNING IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN OVERKOD.LONKODTEXT IN FRAME DIALOG-3
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN OVERKOD.MULTIP IN FRAME DIALOG-3
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN OVERKOD.OVERTIDTILL IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN OVERKOD.VILART IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-3
/* Query rebuild information for DIALOG-BOX DIALOG-3
     _TblList          = "rt8.OVERKOD"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-3 */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_AVBRYT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_AVBRYT DIALOG-3
ON CHOOSE OF BTN_AVBRYT IN FRAME DIALOG-3 /* AVBRYT */
DO:
  {muswait.i}
  musz = TRUE.
  IF bestreco = ? THEN DO:
    FIND FIRST OVERKOD WHERE RECID(OVERKOD) = brec EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE OVERKOD THEN DELETE OVERKOD.
  END.   
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-3
ON CHOOSE OF BTN_OK IN FRAME DIALOG-3 /* OK */
DO:
  {muswait.i}
  DO TRANSACTION:
    ASSIGN {&FIELDS-IN-QUERY-{&FRAME-NAME}}.
    APPLY "GO" TO FRAME {&FRAME-NAME}.    
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
   IF BESTRECO = ? THEN DO TRANSACTION:
     CREATE OVERKOD.
     ASSIGN FRAME {&FRAME-NAME}:TITLE = "NY".
     brec = RECID(OVERKOD).
   END.
   ELSE DO TRANSACTION:
     FIND OVERKOD WHERE RECID(OVERKOD) = BESTRECO EXCLUSIVE-LOCK.
     ASSIGN FRAME {&FRAME-NAME}:TITLE = "ÄNDRA-" + OVERKOD.OVERTIDTILL.
     brec = RECID(OVERKOD).
   END.    
  RUN enable_UI.
  {musarrow.i}
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-3 _DEFAULT-DISABLE
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


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-3 _DEFAULT-ENABLE
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
  IF AVAILABLE OVERKOD THEN 
    DISPLAY OVERKOD.OVERTIDTILL OVERKOD.KOD OVERKOD.VILART OVERKOD.LONKODTEXT 
          OVERKOD.OVERTIDUTTAG OVERKOD.ERSATTNING OVERKOD.ENKEL OVERKOD.ENHET 
          OVERKOD.MULTIP 
      WITH FRAME DIALOG-3.
  ENABLE RECT-12 RECT-11 OVERKOD.OVERTIDTILL OVERKOD.KOD OVERKOD.VILART 
         OVERKOD.LONKODTEXT OVERKOD.OVERTIDUTTAG OVERKOD.ERSATTNING 
         OVERKOD.ENKEL OVERKOD.ENHET OVERKOD.MULTIP BTN_OK BTN_AVBRYT 
      WITH FRAME DIALOG-3.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-3}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


