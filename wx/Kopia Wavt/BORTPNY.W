&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r10 GUI
&ANALYZE-RESUME
/* Connected Databases 
          rt               PROGRESS
*/
&Scoped-define WINDOW-NAME    DIALOG-3
&Scoped-define FRAME-NAME     DIALOG-3
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



/* ********************  Preprocessor Definitions  ******************** */

/* Name of first Frame and/or Browse (alphabetically)                   */
&Scoped-define FRAME-NAME  DIALOG-3

/* Custom List Definitions                                              */
&Scoped-define LIST-1 
&Scoped-define LIST-2 
&Scoped-define LIST-3 

/* Definitions for DIALOG-BOX DIALOG-3                                  */
&Scoped-define FIELDS-IN-QUERY-DIALOG-3 BORTPERS.PERSONALKOD ~
BORTPERS.EFTERNAMN BORTPERS.FORNAMN BORTPERS.PERSONNUMMER ~
BORTPERS.OMRADE BORTPERS.PERSMASK BORTPERS.DATUM ~
BORTPERS.ANSTALLNING 
&Scoped-define OPEN-QUERY-DIALOG-3 OPEN QUERY DIALOG-3 FOR EACH BORTPERS SHARE-LOCK.
&Scoped-define FIRST-TABLE-IN-QUERY-DIALOG-3 BORTPERS
&Scoped-define TABLES-IN-QUERY-DIALOG-3 BORTPERS 

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
     SIZE 47 BY 9.5
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 47.5 BY 3
     BGCOLOR 8 .


/* Query definitions                                                    */
DEFINE QUERY DIALOG-3 FOR BORTPERS SCROLLING.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-3
     BORTPERS.PERSONALKOD AT ROW 2.5 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
          BGCOLOR 8 
     BORTPERS.EFTERNAMN AT ROW 3.5 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
          BGCOLOR 8 
     BORTPERS.FORNAMN AT ROW 4.5 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
          BGCOLOR 8 
     BORTPERS.PERSONNUMMER AT ROW 5.5 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 8 
     BORTPERS.OMRADE AT ROW 6.5 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
          BGCOLOR 8 
     BORTPERS.PERSMASK AT ROW 7.5 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
          BGCOLOR 8 
     BORTPERS.DATUM AT ROW 8.5 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
          BGCOLOR 8 
     BORTPERS.ANSTALLNING AT ROW 9.5 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
          BGCOLOR 8 
     BTN_OK AT ROW 13 COL 10.5
     BTN_AVBRYT AT ROW 13 COL 25
     RECT-11 AT ROW 2 COL 2
     RECT-12 AT ROW 12 COL 1.5
     SPACE(11.13) SKIP(0.59)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE 
         TITLE "Bortpers":L.

 


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-3
  VISIBLE,L                                                             */
ASSIGN 
       FRAME DIALOG-3:SCROLLABLE       = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME



/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-3
/* Query rebuild information for DIALOG-BOX DIALOG-3
     _TblList          = "rt.BORTPERS"
     _Options          = "SHARE-LOCK"
     _OrdList          = ""
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
    FIND FIRST BORTPERS WHERE RECID(BORTPERS) = brec EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE BORTPERS THEN DELETE BORTPERS.
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
     CREATE BORTPERS.
     ASSIGN FRAME {&FRAME-NAME}:TITLE = "NY".
     brec = RECID(BORTPERS).
   END.
   ELSE DO TRANSACTION:
     FIND BORTPERS WHERE RECID(BORTPERS) = BESTRECO EXCLUSIVE-LOCK.
     ASSIGN FRAME {&FRAME-NAME}:TITLE = "?NDRA-" + BORTPERS.ANSTALLNING.
     brec = RECID(BORTPERS).
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
/* --------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
   -------------------------------------------------------------------- */
  /* Hide all frames. */
  HIDE FRAME DIALOG-3.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-3 _DEFAULT-ENABLE
PROCEDURE enable_UI :
/* --------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
   -------------------------------------------------------------------- */
  IF AVAILABLE BORTPERS THEN 
    DISPLAY 
        BORTPERS.PERSONALKOD BORTPERS.EFTERNAMN BORTPERS.FORNAMN 
        BORTPERS.PERSONNUMMER BORTPERS.OMRADE BORTPERS.PERSMASK BORTPERS.DATUM BORTPERS.ANSTALLNING 
      WITH FRAME DIALOG-3.
  ENABLE 
        RECT-11 BORTPERS.PERSONALKOD BORTPERS.EFTERNAMN 
        BORTPERS.FORNAMN BORTPERS.PERSONNUMMER BORTPERS.OMRADE 
        BORTPERS.PERSMASK BORTPERS.DATUM BORTPERS.ANSTALLNING RECT-12 BTN_OK BTN_AVBRYT 
      WITH FRAME DIALOG-3.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-3}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE FRAME-NAME 
&UNDEFINE WINDOW-NAME
