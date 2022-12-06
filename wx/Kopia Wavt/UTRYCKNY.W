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
&Scoped-define FIELDS-IN-QUERY-DIALOG-3 UTRYCKNING.KOD ~
UTRYCKNING.HALV UTRYCKNING.UTRYCKNBER UTRYCKNING.INTERNAT ~
UTRYCKNING.UTRYCKNEJBER UTRYCKNING.LAGOVER UTRYCKNING.UTRYCK1 ~
UTRYCKNING.PARA8 UTRYCKNING.KLOCKAN UTRYCKNING.OVLAPP ~
UTRYCKNING.BER2 UTRYCKNING.OB UTRYCKNING.EJBER2 ~
UTRYCKNING.NODF UTRYCKNING.AVBE UTRYCKNING.LUFT ~
UTRYCKNING.ENDMALTID UTRYCKNING.RHALV UTRYCKNING.EXAKTM ~
UTRYCKNING.EXTRAARBPASS UTRYCKNING.EXTID 
&Scoped-define OPEN-QUERY-DIALOG-3 OPEN QUERY DIALOG-3 FOR EACH UTRYCKNING SHARE-LOCK.
&Scoped-define FIRST-TABLE-IN-QUERY-DIALOG-3 UTRYCKNING
&Scoped-define TABLES-IN-QUERY-DIALOG-3 UTRYCKNING 

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
     SIZE 47 BY 12.5
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 44 BY 2
     BGCOLOR 8 .


/* Query definitions                                                    */
DEFINE QUERY DIALOG-3 FOR UTRYCKNING SCROLLING.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-3
     UTRYCKNING.KOD AT ROW 2.5 COL 16.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3 BY 1
          BGCOLOR 8 
     UTRYCKNING.HALV AT ROW 2.5 COL 39.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
          BGCOLOR 8 
     UTRYCKNING.UTRYCKNBER AT ROW 3.5 COL 16.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
          BGCOLOR 8 
     UTRYCKNING.INTERNAT AT ROW 3.5 COL 39.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
          BGCOLOR 8 
     UTRYCKNING.UTRYCKNEJBER AT ROW 4.5 COL 16.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
          BGCOLOR 8 
     UTRYCKNING.LAGOVER AT ROW 4.5 COL 39.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
          BGCOLOR 8 
     UTRYCKNING.UTRYCK1 AT ROW 5.5 COL 16.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
          BGCOLOR 8 
     UTRYCKNING.PARA8 AT ROW 5.5 COL 39.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
          BGCOLOR 8 
     UTRYCKNING.KLOCKAN AT ROW 6.5 COL 16.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
          BGCOLOR 8 
     UTRYCKNING.OVLAPP AT ROW 6.5 COL 39.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
          BGCOLOR 8 
     UTRYCKNING.BER2 AT ROW 7.5 COL 16.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
          BGCOLOR 8 
     UTRYCKNING.OB AT ROW 7.5 COL 39.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
          BGCOLOR 8 
     UTRYCKNING.EJBER2 AT ROW 8.5 COL 16.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
          BGCOLOR 8 
     UTRYCKNING.NODF AT ROW 8.5 COL 39.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
          BGCOLOR 8 
     UTRYCKNING.AVBE AT ROW 9.5 COL 16.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
          BGCOLOR 8 
     UTRYCKNING.LUFT AT ROW 9.5 COL 39.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
          BGCOLOR 8 
     UTRYCKNING.ENDMALTID AT ROW 10.5 COL 16.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
          BGCOLOR 8 
     UTRYCKNING.RHALV AT ROW 10.5 COL 39.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
          BGCOLOR 8 
     UTRYCKNING.EXAKTM AT ROW 11.5 COL 16.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
          BGCOLOR 8 
     UTRYCKNING.EXTRAARBPASS AT ROW 11.5 COL 39.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5 BY 1
          BGCOLOR 8 
     UTRYCKNING.EXTID AT ROW 12.5 COL 16.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
          BGCOLOR 8 
     BTN_OK AT ROW 15.5 COL 11.5
     BTN_AVBRYT AT ROW 15.5 COL 26
     RECT-11 AT ROW 2 COL 2
     RECT-12 AT ROW 15 COL 2.5
     SPACE(5.13) SKIP(0.68)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE 
         TITLE "Utryckning":L.

 


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
     _TblList          = "rt.UTRYCKNING"
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
    FIND FIRST UTRYCKNING WHERE RECID(UTRYCKNING) = brec EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE UTRYCKNING THEN DELETE UTRYCKNING.
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
     CREATE UTRYCKNING.
     ASSIGN FRAME {&FRAME-NAME}:TITLE = "NY".
     brec = RECID(UTRYCKNING).
   END.
   ELSE DO TRANSACTION:
     FIND UTRYCKNING WHERE RECID(UTRYCKNING) = BESTRECO EXCLUSIVE-LOCK.
     ASSIGN FRAME {&FRAME-NAME}:TITLE = "ÄNDRA-" + UTRYCKNING.KOD.
     brec = RECID(UTRYCKNING).
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
  IF AVAILABLE UTRYCKNING THEN 
    DISPLAY 
        UTRYCKNING.KOD UTRYCKNING.HALV UTRYCKNING.UTRYCKNBER 
        UTRYCKNING.INTERNAT UTRYCKNING.UTRYCKNEJBER 
        UTRYCKNING.LAGOVER UTRYCKNING.UTRYCK1 UTRYCKNING.PARA8 
        UTRYCKNING.KLOCKAN UTRYCKNING.OVLAPP UTRYCKNING.BER2 
        UTRYCKNING.OB UTRYCKNING.EJBER2 UTRYCKNING.NODF 
        UTRYCKNING.AVBE UTRYCKNING.LUFT UTRYCKNING.ENDMALTID 
        UTRYCKNING.RHALV UTRYCKNING.EXAKTM UTRYCKNING.EXTRAARBPASS UTRYCKNING.EXTID 
      WITH FRAME DIALOG-3.
  ENABLE 
        RECT-11 UTRYCKNING.KOD UTRYCKNING.HALV UTRYCKNING.UTRYCKNBER 
        UTRYCKNING.INTERNAT UTRYCKNING.UTRYCKNEJBER 
        UTRYCKNING.LAGOVER UTRYCKNING.UTRYCK1 UTRYCKNING.PARA8 
        UTRYCKNING.KLOCKAN UTRYCKNING.OVLAPP UTRYCKNING.BER2 
        UTRYCKNING.OB UTRYCKNING.EJBER2 UTRYCKNING.NODF 
        UTRYCKNING.AVBE UTRYCKNING.LUFT UTRYCKNING.ENDMALTID 
        UTRYCKNING.RHALV UTRYCKNING.EXAKTM UTRYCKNING.EXTRAARBPASS UTRYCKNING.EXTID RECT-12 BTN_OK BTN_AVBRYT 
      WITH FRAME DIALOG-3.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-3}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE FRAME-NAME 
&UNDEFINE WINDOW-NAME
