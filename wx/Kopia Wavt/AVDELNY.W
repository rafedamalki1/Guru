&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r11 GUI
&ANALYZE-RESUME
/* Connected Databases 
          RT8              PROGRESS
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
&Scoped-define FIELDS-IN-QUERY-DIALOG-3 AVDELNING.AVDELNINGNR ~
AVDELNING.AVDELNINGNAMN AVDELNING.ELVOMRKOD AVDELNING.GATUADR ~
AVDELNING.POSTANST AVDELNING.POSTNR AVDELNING.POSTADR AVDELNING.TELVXL ~
AVDELNING.TELJOUR AVDELNING.ANTALAB AVDELNING.ANTALANST ~
AVDELNING.BUDGRAMINV AVDELNING.BUDGRAMUH AVDELNING.NUAK ~
AVDELNING.KOSTNTYPUH AVDELNING.KOSTNTYPINV AVDELNING.KONTOA ~
AVDELNING.KONTOB AVDELNING.KONTOC AVDELNING.KONTOD AVDELNING.KOSTPERS ~
AVDELNING.KOSTMASK 
&Scoped-define ENABLED-FIELDS-IN-QUERY-DIALOG-3 AVDELNING.AVDELNINGNR ~
AVDELNING.AVDELNINGNAMN AVDELNING.ELVOMRKOD AVDELNING.GATUADR ~
AVDELNING.POSTANST AVDELNING.POSTNR AVDELNING.POSTADR AVDELNING.TELVXL ~
AVDELNING.TELJOUR AVDELNING.ANTALAB AVDELNING.ANTALANST ~
AVDELNING.BUDGRAMINV AVDELNING.BUDGRAMUH AVDELNING.NUAK AVDELNING.KOSTNTYPUH ~
AVDELNING.KOSTNTYPINV AVDELNING.KONTOA AVDELNING.KONTOB AVDELNING.KONTOC ~
AVDELNING.KONTOD AVDELNING.KOSTPERS AVDELNING.KOSTMASK 
&Scoped-define OPEN-QUERY-DIALOG-3 OPEN QUERY DIALOG-3 FOR EACH AVDELNING SHARE-LOCK.
&Scoped-define FIRST-TABLE-IN-QUERY-DIALOG-3 AVDELNING
&Scoped-define TABLES-IN-QUERY-DIALOG-3 AVDELNING 

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
     SIZE 71.75 BY 18.09
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 71.75 BY 3.14
     BGCOLOR 8 .


/* Query definitions                                                    */
DEFINE QUERY DIALOG-3 FOR AVDELNING SCROLLING.

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-3
     AVDELNING.AVDELNINGNR AT ROW 1.55 COL 15.63 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3 BY 1
          BGCOLOR 8 
     AVDELNING.AVDELNINGNAMN AT ROW 1.64 COL 31.63 COLON-ALIGNED FORMAT "x(35)"
          VIEW-AS FILL-IN 
          SIZE 36.5 BY 1.14
          BGCOLOR 8 
     AVDELNING.ELVOMRKOD AT ROW 2.82 COL 15.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 8 
     AVDELNING.GATUADR AT ROW 4.05 COL 15.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
          BGCOLOR 8 
     AVDELNING.POSTANST AT ROW 5.27 COL 41.38 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
          BGCOLOR 8 
     AVDELNING.POSTNR AT ROW 5.32 COL 15.63 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
          BGCOLOR 8 
     AVDELNING.POSTADR AT ROW 6.45 COL 41.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 26 BY 1
          BGCOLOR 8 
     AVDELNING.TELVXL AT ROW 6.59 COL 15.63 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 8 
     AVDELNING.TELJOUR AT ROW 7.86 COL 15.75 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 12 BY 1
          BGCOLOR 8 
     AVDELNING.ANTALAB AT ROW 9.41 COL 20.13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 8 
     AVDELNING.ANTALANST AT ROW 9.5 COL 52.25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 8 
     AVDELNING.BUDGRAMINV AT ROW 10.64 COL 20.13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 8 
     AVDELNING.BUDGRAMUH AT ROW 10.64 COL 52.25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 8 
     AVDELNING.NUAK AT ROW 11.86 COL 20.13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 8 
     AVDELNING.KOSTNTYPUH AT ROW 13.14 COL 20.13 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
          BGCOLOR 8 
     AVDELNING.KOSTNTYPINV AT ROW 13.27 COL 52.63 COLON-ALIGNED HELP
          "KOSTNADSTYPSGRUPP FÖR INVESTERING."
          LABEL "KOSTNADSTYPSGRUPP inv"
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
          BGCOLOR 8 
     AVDELNING.KONTOA AT ROW 14.36 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
          BGCOLOR 8 
     AVDELNING.KONTOB AT ROW 14.5 COL 52.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
          BGCOLOR 8 
     AVDELNING.KONTOC AT ROW 15.59 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
          BGCOLOR 8 
     AVDELNING.KONTOD AT ROW 15.82 COL 52.63 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
          BGCOLOR 8 
     AVDELNING.KOSTPERS AT ROW 16.86 COL 19.88 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 8 
     AVDELNING.KOSTMASK AT ROW 17.09 COL 52.5 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 8 
     BTN_OK AT ROW 20.5 COL 22.25
     BTN_AVBRYT AT ROW 20.5 COL 36.75
     RECT-11 AT ROW 1.14 COL 1
     RECT-12 AT ROW 19.27 COL 1
     SPACE(1.12) SKIP(0.08)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D SCROLLABLE 
         TITLE "Avdelning":L.

 


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
ASSIGN 
       FRAME DIALOG-3:SCROLLABLE       = FALSE.

/* SETTINGS FOR FILL-IN AVDELNING.AVDELNINGNAMN IN FRAME DIALOG-3
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN AVDELNING.KOSTNTYPINV IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-3
/* Query rebuild information for DIALOG-BOX DIALOG-3
     _TblList          = "RT8.AVDELNING"
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
    FIND FIRST AVDELNING WHERE RECID(AVDELNING) = brec EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE AVDELNING THEN DELETE AVDELNING.
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
     CREATE AVDELNING.
     ASSIGN FRAME {&FRAME-NAME}:TITLE = "NY".
     brec = RECID(AVDELNING).
   END.
   ELSE DO TRANSACTION:
     FIND AVDELNING WHERE RECID(AVDELNING) = BESTRECO EXCLUSIVE-LOCK.
     ASSIGN FRAME {&FRAME-NAME}:TITLE = "ÄNDRA-" + AVDELNING.AVDELNINGNAMN.
     brec = RECID(AVDELNING).
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
  IF AVAILABLE AVDELNING THEN 
    DISPLAY AVDELNING.AVDELNINGNR AVDELNING.AVDELNINGNAMN AVDELNING.ELVOMRKOD 
          AVDELNING.GATUADR AVDELNING.POSTANST AVDELNING.POSTNR 
          AVDELNING.POSTADR AVDELNING.TELVXL AVDELNING.TELJOUR AVDELNING.ANTALAB 
          AVDELNING.ANTALANST AVDELNING.BUDGRAMINV AVDELNING.BUDGRAMUH 
          AVDELNING.NUAK AVDELNING.KOSTNTYPUH AVDELNING.KOSTNTYPINV 
          AVDELNING.KONTOA AVDELNING.KONTOB AVDELNING.KONTOC AVDELNING.KONTOD 
          AVDELNING.KOSTPERS AVDELNING.KOSTMASK 
      WITH FRAME DIALOG-3.
  ENABLE RECT-11 AVDELNING.AVDELNINGNR AVDELNING.AVDELNINGNAMN 
         AVDELNING.ELVOMRKOD AVDELNING.GATUADR AVDELNING.POSTANST 
         AVDELNING.POSTNR AVDELNING.POSTADR AVDELNING.TELVXL AVDELNING.TELJOUR 
         AVDELNING.ANTALAB AVDELNING.ANTALANST AVDELNING.BUDGRAMINV 
         AVDELNING.BUDGRAMUH AVDELNING.NUAK AVDELNING.KOSTNTYPUH 
         AVDELNING.KOSTNTYPINV AVDELNING.KONTOA AVDELNING.KONTOB 
         AVDELNING.KONTOC AVDELNING.KONTOD AVDELNING.KOSTPERS 
         AVDELNING.KOSTMASK RECT-12 BTN_OK BTN_AVBRYT 
      WITH FRAME DIALOG-3.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-3}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE FRAME-NAME
&UNDEFINE WINDOW-NAME
