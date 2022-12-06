&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          rt9              PROGRESS
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
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES LONTILL

/* Definitions for DIALOG-BOX DIALOG-3                                  */
&Scoped-define FIELDS-IN-QUERY-DIALOG-3 LONTILL.KOD LONTILL.LONTILLAGG ~
LONTILL.VILART LONTILL.LONKODTEXT LONTILL.ENHET LONTILL.ERSATTNING ~
LONTILL.LONTILLANTAL LONTILL.AONROBL LONTILL.MULTIP LONTILL.TYPKOD ~
LONTILL.VALBAR 
&Scoped-define ENABLED-FIELDS-IN-QUERY-DIALOG-3 LONTILL.KOD ~
LONTILL.LONTILLAGG LONTILL.VILART LONTILL.LONKODTEXT LONTILL.ENHET ~
LONTILL.ERSATTNING LONTILL.LONTILLANTAL LONTILL.AONROBL LONTILL.MULTIP ~
LONTILL.TYPKOD LONTILL.VALBAR 
&Scoped-define ENABLED-TABLES-IN-QUERY-DIALOG-3 LONTILL
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-DIALOG-3 LONTILL
&Scoped-define QUERY-STRING-DIALOG-3 FOR EACH LONTILL SHARE-LOCK
&Scoped-define OPEN-QUERY-DIALOG-3 OPEN QUERY DIALOG-3 FOR EACH LONTILL SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DIALOG-3 LONTILL
&Scoped-define FIRST-TABLE-IN-QUERY-DIALOG-3 LONTILL


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS LONTILL.KOD LONTILL.LONTILLAGG LONTILL.VILART ~
LONTILL.LONKODTEXT LONTILL.ENHET LONTILL.ERSATTNING LONTILL.LONTILLANTAL ~
LONTILL.AONROBL LONTILL.MULTIP LONTILL.TYPKOD LONTILL.VALBAR 
&Scoped-define ENABLED-TABLES LONTILL
&Scoped-define FIRST-ENABLED-TABLE LONTILL
&Scoped-Define ENABLED-OBJECTS FILL-IN-2 BTN_OK BTN_AVBRYT 
&Scoped-Define DISPLAYED-FIELDS LONTILL.KOD LONTILL.LONTILLAGG ~
LONTILL.VILART LONTILL.LONKODTEXT LONTILL.ENHET LONTILL.ERSATTNING ~
LONTILL.LONTILLANTAL LONTILL.AONROBL LONTILL.MULTIP LONTILL.TYPKOD ~
LONTILL.VALBAR 
&Scoped-define DISPLAYED-TABLES LONTILL
&Scoped-define FIRST-DISPLAYED-TABLE LONTILL
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-2 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_AVBRYT AUTO-END-KEY 
     LABEL "AVBRYT":L 
     SIZE 11 BY 1.17.

DEFINE BUTTON BTN_OK 
     LABEL "OK":L 
     SIZE 10.5 BY 1.17.

DEFINE VARIABLE FILL-IN-2 AS LOGICAL FORMAT "yes/no":U INITIAL NO 
     LABEL "RESKOD" 
     VIEW-AS FILL-IN 
     SIZE 5.5 BY 1
     BGCOLOR 8  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DIALOG-3 FOR 
      LONTILL SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-3
     LONTILL.KOD AT ROW 2.5 COL 19 COLON-ALIGNED
          LABEL "KOD" FORMAT "x(2)"
          VIEW-AS FILL-IN 
          SIZE 3 BY 1
          BGCOLOR 8 
     LONTILL.LONTILLAGG AT ROW 3.5 COL 19 COLON-ALIGNED
          LABEL "LÖNETILLÄGGKOD" FORMAT "x(5)"
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
          BGCOLOR 8 
     LONTILL.VILART AT ROW 4.67 COL 18.75 COLON-ALIGNED HELP
          ""
          LABEL "VISAD LÖNEART" FORMAT "X(8)"
          VIEW-AS FILL-IN 
          SIZE 7.13 BY .92
          BGCOLOR 8 
     LONTILL.LONKODTEXT AT ROW 5.75 COL 18.75 COLON-ALIGNED HELP
          ""
          LABEL "LONKODTEXT" FORMAT "x(45)"
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
          BGCOLOR 8 
     LONTILL.ENHET AT ROW 6.75 COL 18.75 COLON-ALIGNED HELP
          ""
          LABEL "SORT" FORMAT "x(2)"
          VIEW-AS FILL-IN 
          SIZE 3 BY 1
          BGCOLOR 8 
     LONTILL.ERSATTNING AT ROW 7.75 COL 18.75 COLON-ALIGNED HELP
          ""
          LABEL "ERSÄTTNING I KR." FORMAT "->>>>9.99"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 8 
     LONTILL.LONTILLANTAL AT ROW 8.75 COL 18.75 COLON-ALIGNED HELP
          ""
          LABEL "ANTAL" FORMAT ">>>9.<<"
          VIEW-AS FILL-IN 
          SIZE 6 BY 1
          BGCOLOR 8 
     LONTILL.AONROBL AT ROW 9.75 COL 18.75 COLON-ALIGNED HELP
          ""
          LABEL "AONROBL" FORMAT "yes/no"
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
          BGCOLOR 8 
     FILL-IN-2 AT ROW 11.25 COL 18.25 COLON-ALIGNED
     LONTILL.MULTIP AT ROW 12.75 COL 18.25 COLON-ALIGNED HELP
          ""
          LABEL "MULTIP" FORMAT "->>>>9.999"
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 8 
     LONTILL.TYPKOD AT ROW 13.75 COL 18.25 COLON-ALIGNED HELP
          ""
          LABEL "TYPKOD" FORMAT "X(13)"
          VIEW-AS FILL-IN 
          SIZE 14 BY 1
          BGCOLOR 8 
     LONTILL.VALBAR AT ROW 15.21 COL 18.5 COLON-ALIGNED HELP
          ""
          LABEL "VALBAR" FORMAT "yes/no"
          VIEW-AS FILL-IN 
          SIZE 4.25 BY 1
     BTN_OK AT ROW 19.08 COL 13.5
     BTN_AVBRYT AT ROW 19.08 COL 26
     SPACE(12.62) SKIP(1.07)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Lontill":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-3
                                                                        */
ASSIGN 
       FRAME DIALOG-3:SCROLLABLE       = FALSE.

/* SETTINGS FOR FILL-IN LONTILL.AONROBL IN FRAME DIALOG-3
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN LONTILL.ENHET IN FRAME DIALOG-3
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN LONTILL.ERSATTNING IN FRAME DIALOG-3
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN LONTILL.FILL-IN-VALBAR IN FRAME DIALOG-3
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN LONTILL.KOD IN FRAME DIALOG-3
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN LONTILL.LONKODTEXT IN FRAME DIALOG-3
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN LONTILL.LONTILLAGG IN FRAME DIALOG-3
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN LONTILL.LONTILLANTAL IN FRAME DIALOG-3
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN LONTILL.MULTIP IN FRAME DIALOG-3
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN LONTILL.TYPKOD IN FRAME DIALOG-3
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN LONTILL.VILART IN FRAME DIALOG-3
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-3
/* Query rebuild information for DIALOG-BOX DIALOG-3
     _TblList          = "rt9.LONTILL"
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
    FIND FIRST LONTILL WHERE RECID(LONTILL) = brec EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE LONTILL THEN DELETE LONTILL.
  END.   
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-3
ON CHOOSE OF BTN_OK IN FRAME DIALOG-3 /* OK */
DO:
  {muswait.i}
  IF INPUT LONTILL.LONTILLAGG = "" THEN DO:
    MESSAGE "Lönetillägg får ej vara blankt".
    RETURN NO-APPLY.
  END.  
  DO TRANSACTION:
    ASSIGN {&FIELDS-IN-QUERY-{&FRAME-NAME}}.
    APPLY "GO" TO FRAME {&FRAME-NAME}.    
  END.
  IF FILL-IN-2 = TRUE THEN DO:
     FIND FIRST RESTAB WHERE RESTAB.RESKOD = LONTILL.LONTILLAGG USE-INDEX RESTAB
     NO-LOCK NO-ERROR.
     IF NOT AVAILABLE RESTAB THEN DO TRANSACTION:
        CREATE RESTAB.
        ASSIGN RESTAB.RESKOD = LONTILL.LONTILLAGG.
     END.
  END.
  ELSE DO:
     FIND FIRST LONETAB WHERE LONETAB.LONTILLAGG = LONTILL.LONTILLAGG USE-INDEX LONETAB
     NO-LOCK NO-ERROR.
     IF NOT AVAILABLE LONETAB THEN DO TRANSACTION:
        CREATE LONETAB.
        ASSIGN LONETAB.LONTILLAGG = LONTILL.LONTILLAGG.
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
   IF BESTRECO = ? THEN DO TRANSACTION:
     CREATE LONTILL.
     ASSIGN FRAME {&FRAME-NAME}:TITLE = "NY".
     brec = RECID(LONTILL).
     ASSIGN LONTILL.VALBAR = TRUE.
   END.
   ELSE DO TRANSACTION:
     FIND LONTILL WHERE RECID(LONTILL) = BESTRECO EXCLUSIVE-LOCK.
     ASSIGN FRAME {&FRAME-NAME}:TITLE = "ÄNDRA-" + LONTILL.LONTILLAGG.
     brec = RECID(LONTILL).
   END.    
  RUN enable_UI.
  {musarrow.i}
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
  DISPLAY FILL-IN-2 
      WITH FRAME DIALOG-3.
  IF AVAILABLE LONTILL THEN 
    DISPLAY LONTILL.KOD LONTILL.LONTILLAGG LONTILL.VILART LONTILL.LONKODTEXT 
          LONTILL.ENHET LONTILL.ERSATTNING LONTILL.LONTILLANTAL LONTILL.AONROBL 
          LONTILL.MULTIP LONTILL.TYPKOD LONTILL.VALBAR 
      WITH FRAME DIALOG-3.
  ENABLE LONTILL.KOD LONTILL.LONTILLAGG LONTILL.VILART LONTILL.LONKODTEXT 
         LONTILL.ENHET LONTILL.ERSATTNING LONTILL.LONTILLANTAL LONTILL.AONROBL 
         FILL-IN-2 LONTILL.MULTIP LONTILL.TYPKOD LONTILL.VALBAR BTN_OK 
         BTN_AVBRYT 
      WITH FRAME DIALOG-3.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-3}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

