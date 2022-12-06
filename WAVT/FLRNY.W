&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v7r10 GUI
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

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES FLEXREG

/* Definitions for DIALOG-BOX DIALOG-3                                  */
&Scoped-define QUERY-STRING-DIALOG-3 FOR EACH FLEXREG SHARE-LOCK
&Scoped-define OPEN-QUERY-DIALOG-3 OPEN QUERY DIALOG-3 FOR EACH FLEXREG SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DIALOG-3 FLEXREG
&Scoped-define FIRST-TABLE-IN-QUERY-DIALOG-3 FLEXREG


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN_KOD FILL-IN_SOST FILL-IN_SOSL ~
FILL-IN_MOSTART FILL-IN_MOSLUT FILL-IN_SALDOKORD FILL-IN_KVSTART ~
FILL-IN_KVSLUT FILL-IN_KVSOST FILL-IN_KVSOSLUT FILL-IN_MINSTA ~
FILL-IN_MINSLU BTN_OK BTN_AVBRYT 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN_KOD FILL-IN_SOST FILL-IN_SOSL ~
FILL-IN_MOSTART FILL-IN_MOSLUT FILL-IN_SALDOKORD FILL-IN_KVSTART ~
FILL-IN_KVSLUT FILL-IN_KVSOST FILL-IN_KVSOSLUT FILL-IN_MINSTA ~
FILL-IN_MINSLU 

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

DEFINE VARIABLE FILL-IN_KOD AS CHARACTER FORMAT "x(8)" 
     LABEL "KOD" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN_KVSLUT AS DECIMAL FORMAT ">9.99" INITIAL 0 
     LABEL "KVSLUT" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_KVSOSLUT AS DECIMAL FORMAT ">9.99" INITIAL 0 
     LABEL "KVSOSLUT" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_KVSOST AS DECIMAL FORMAT ">9.99" INITIAL 0 
     LABEL "KVSOSTART" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_KVSTART AS DECIMAL FORMAT ">9.99" INITIAL 0 
     LABEL "KVSTART" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_MINSLU AS DECIMAL FORMAT ">9.99" INITIAL 0 
     LABEL "MINSLU" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_MINSTA AS DECIMAL FORMAT ">9.99" INITIAL 0 
     LABEL "MINSTA" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_MOSLUT AS DECIMAL FORMAT ">9.99" INITIAL 0 
     LABEL "MOSLUT" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_MOSTART AS DECIMAL FORMAT ">9.99" INITIAL 0 
     LABEL "MOSTART" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1.

DEFINE VARIABLE FILL-IN_SALDOKORD AS DATE FORMAT "99/99/99" 
     LABEL "SALDOKÖRD" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN_SOSL AS DATE FORMAT "99/99/99" 
     LABEL "SOMMARSL" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1.

DEFINE VARIABLE FILL-IN_SOST AS DATE FORMAT "99/99/99" INITIAL ? 
     LABEL "SOMMARST" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DIALOG-3 FOR 
      FLEXREG SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-3
     FILL-IN_KOD AT ROW 1.75 COL 15.5 COLON-ALIGNED
     FILL-IN_SOST AT ROW 3 COL 15.5 COLON-ALIGNED
     FILL-IN_SOSL AT ROW 4.25 COL 15.5 COLON-ALIGNED
     FILL-IN_MOSTART AT ROW 5.5 COL 15.5 COLON-ALIGNED
     FILL-IN_MOSLUT AT ROW 6.75 COL 15.5 COLON-ALIGNED
     FILL-IN_SALDOKORD AT ROW 8 COL 15.5 COLON-ALIGNED
     FILL-IN_KVSTART AT ROW 9.25 COL 15.5 COLON-ALIGNED
     FILL-IN_KVSLUT AT ROW 10.5 COL 15.5 COLON-ALIGNED
     FILL-IN_KVSOST AT ROW 11.75 COL 15.5 COLON-ALIGNED
     FILL-IN_KVSOSLUT AT ROW 13 COL 15.5 COLON-ALIGNED
     FILL-IN_MINSTA AT ROW 14.25 COL 15.5 COLON-ALIGNED
     FILL-IN_MINSLU AT ROW 15.25 COL 15.5 COLON-ALIGNED
     BTN_OK AT ROW 21.04 COL 10.5
     BTN_AVBRYT AT ROW 21.04 COL 25
     SPACE(14.99) SKIP(0.36)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "FLEXREG":L.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-3
                                                                        */
ASSIGN 
       FRAME DIALOG-3:SCROLLABLE       = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-3
/* Query rebuild information for DIALOG-BOX DIALOG-3
     _TblList          = "RT9.FLEXREG"
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
    FIND FIRST FLEXREG WHERE RECID(FLEXREG) = brec EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE FLEXREG THEN DELETE FLEXREG.
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
    ASSIGN
    FLEXREG.KOD = INPUT FILL-IN_KOD
    FLEXREG.SOMMARST = INPUT FILL-IN_SOST
    FLEXREG.SOMMARSL = INPUT FILL-IN_SOSL
    FLEXREG.MOSTART =  INPUT FILL-IN_MOSTART
    FLEXREG.MOSLUT =  INPUT FILL-IN_MOSLUT
    FLEXREG.SALDOKORD = INPUT FILL-IN_SALDOKORD
    FLEXREG.KVSTART = INPUT FILL-IN_KVSTART
    FLEXREG.KVSLUT =  INPUT FILL-IN_KVSLUT 
    FLEXREG.KVSOST =  INPUT FILL-IN_KVSOST 
    FLEXREG.KVSOSL =  INPUT FILL-IN_KVSOSLUT 
    FLEXREG.MINSTA =  INPUT FILL-IN_MINSTA 
    FLEXREG.MINSLU =  INPUT FILL-IN_MINSLU. 
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
     CREATE FLEXREG.
     ASSIGN FRAME {&FRAME-NAME}:TITLE = "NY".
     brec = RECID(FLEXREG).
   END.
   ELSE DO TRANSACTION:
     FIND FLEXREG WHERE RECID(FLEXREG) = BESTRECO EXCLUSIVE-LOCK.
     ASSIGN FRAME {&FRAME-NAME}:TITLE = "ÄNDRA-" + FLEXREG.KOD.

     ASSIGN
     FILL-IN_KOD = FLEXREG.KOD
     FILL-IN_SOST = FLEXREG.SOMMARST
     FILL-IN_SOSL = FLEXREG.SOMMARSL 
     FILL-IN_MOSTART = FLEXREG.MOSTART 
     FILL-IN_MOSLUT = FLEXREG.MOSLUT 
     FILL-IN_SALDOKORD = FLEXREG.SALDOKORD 
     FILL-IN_KVSTART = FLEXREG.KVSTART 
     FILL-IN_KVSLUT  = FLEXREG.KVSLUT 
     FILL-IN_KVSOST  = FLEXREG.KVSOST 
     FILL-IN_KVSOSLUT = FLEXREG.KVSOSL 
     FILL-IN_MINSTA  = FLEXREG.MINSTA
     FILL-IN_MINSLU = FLEXREG.MINSLU.     
     brec = RECID(FLEXREG).
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
  DISPLAY FILL-IN_KOD FILL-IN_SOST FILL-IN_SOSL FILL-IN_MOSTART FILL-IN_MOSLUT 
          FILL-IN_SALDOKORD FILL-IN_KVSTART FILL-IN_KVSLUT FILL-IN_KVSOST 
          FILL-IN_KVSOSLUT FILL-IN_MINSTA FILL-IN_MINSLU 
      WITH FRAME DIALOG-3.
  ENABLE FILL-IN_KOD FILL-IN_SOST FILL-IN_SOSL FILL-IN_MOSTART FILL-IN_MOSLUT 
         FILL-IN_SALDOKORD FILL-IN_KVSTART FILL-IN_KVSLUT FILL-IN_KVSOST 
         FILL-IN_KVSOSLUT FILL-IN_MINSTA FILL-IN_MINSLU BTN_OK BTN_AVBRYT 
      WITH FRAME DIALOG-3.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-3}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

