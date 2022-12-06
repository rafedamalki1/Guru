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
DEFINE VARIABLE gamlabef LIKE BEFATTNINGSTAB.BEFATTNING NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES BEFATTNINGSTAB

/* Definitions for DIALOG-BOX DIALOG-3                                  */
&Scoped-define FIELDS-IN-QUERY-DIALOG-3 BEFATTNINGSTAB.BEFATTNING ~
BEFATTNINGSTAB.NAMN BEFATTNINGSTAB.PRISA BEFATTNINGSTAB.PERSMASK ~
BEFATTNINGSTAB.BRAVO BEFATTNINGSTAB.AV-LEVEL BEFATTNINGSTAB.KOSTNADSSLAG ~
BEFATTNINGSTAB.BILF BEFATTNINGSTAB.PLUSD 
&Scoped-define ENABLED-FIELDS-IN-QUERY-DIALOG-3 BEFATTNINGSTAB.BEFATTNING ~
BEFATTNINGSTAB.NAMN BEFATTNINGSTAB.PRISA BEFATTNINGSTAB.PERSMASK ~
BEFATTNINGSTAB.BRAVO BEFATTNINGSTAB.AV-LEVEL BEFATTNINGSTAB.KOSTNADSSLAG ~
BEFATTNINGSTAB.BILF BEFATTNINGSTAB.PLUSD 
&Scoped-define ENABLED-TABLES-IN-QUERY-DIALOG-3 BEFATTNINGSTAB
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-DIALOG-3 BEFATTNINGSTAB

&Scoped-define FIELD-PAIRS-IN-QUERY-DIALOG-3~
 ~{&FP1}BEFATTNING ~{&FP2}BEFATTNING ~{&FP3}~
 ~{&FP1}NAMN ~{&FP2}NAMN ~{&FP3}~
 ~{&FP1}PRISA ~{&FP2}PRISA ~{&FP3}~
 ~{&FP1}PERSMASK ~{&FP2}PERSMASK ~{&FP3}~
 ~{&FP1}BRAVO ~{&FP2}BRAVO ~{&FP3}~
 ~{&FP1}AV-LEVEL ~{&FP2}AV-LEVEL ~{&FP3}~
 ~{&FP1}KOSTNADSSLAG ~{&FP2}KOSTNADSSLAG ~{&FP3}~
 ~{&FP1}BILF ~{&FP2}BILF ~{&FP3}~
 ~{&FP1}PLUSD ~{&FP2}PLUSD ~{&FP3}
&Scoped-define OPEN-QUERY-DIALOG-3 OPEN QUERY DIALOG-3 FOR EACH BEFATTNINGSTAB SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DIALOG-3 BEFATTNINGSTAB
&Scoped-define FIRST-TABLE-IN-QUERY-DIALOG-3 BEFATTNINGSTAB


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS BEFATTNINGSTAB.BEFATTNING BEFATTNINGSTAB.NAMN ~
BEFATTNINGSTAB.PRISA BEFATTNINGSTAB.PERSMASK BEFATTNINGSTAB.BRAVO ~
BEFATTNINGSTAB.AV-LEVEL BEFATTNINGSTAB.KOSTNADSSLAG BEFATTNINGSTAB.BILF ~
BEFATTNINGSTAB.PLUSD 
&Scoped-define FIELD-PAIRS~
 ~{&FP1}BEFATTNING ~{&FP2}BEFATTNING ~{&FP3}~
 ~{&FP1}NAMN ~{&FP2}NAMN ~{&FP3}~
 ~{&FP1}PRISA ~{&FP2}PRISA ~{&FP3}~
 ~{&FP1}PERSMASK ~{&FP2}PERSMASK ~{&FP3}~
 ~{&FP1}BRAVO ~{&FP2}BRAVO ~{&FP3}~
 ~{&FP1}AV-LEVEL ~{&FP2}AV-LEVEL ~{&FP3}~
 ~{&FP1}KOSTNADSSLAG ~{&FP2}KOSTNADSSLAG ~{&FP3}~
 ~{&FP1}BILF ~{&FP2}BILF ~{&FP3}~
 ~{&FP1}PLUSD ~{&FP2}PLUSD ~{&FP3}
&Scoped-define ENABLED-TABLES BEFATTNINGSTAB
&Scoped-define FIRST-ENABLED-TABLE BEFATTNINGSTAB
&Scoped-Define ENABLED-OBJECTS RECT-12 RECT-11 BTN_OK BTN_AVBRYT 
&Scoped-Define DISPLAYED-FIELDS BEFATTNINGSTAB.BEFATTNING ~
BEFATTNINGSTAB.NAMN BEFATTNINGSTAB.PRISA BEFATTNINGSTAB.PERSMASK ~
BEFATTNINGSTAB.BRAVO BEFATTNINGSTAB.AV-LEVEL BEFATTNINGSTAB.KOSTNADSSLAG ~
BEFATTNINGSTAB.BILF BEFATTNINGSTAB.PLUSD 

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
     SIZE 44.5 BY 9.91
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 4 GRAPHIC-EDGE  
     SIZE 44.5 BY 3
     BGCOLOR 8 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DIALOG-3 FOR 
      BEFATTNINGSTAB SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-3
     BEFATTNINGSTAB.BEFATTNING AT ROW 1.36 COL 20.25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
          BGCOLOR 8 
     BEFATTNINGSTAB.NAMN AT ROW 2.55 COL 20.25 COLON-ALIGNED
          LABEL "Befattning visa"
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
     BEFATTNINGSTAB.PRISA AT ROW 3.77 COL 20.25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 8 
     BEFATTNINGSTAB.PERSMASK AT ROW 4.77 COL 20.25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
          BGCOLOR 8 
     BEFATTNINGSTAB.BRAVO AT ROW 5.77 COL 20.25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
          BGCOLOR 8 
     BEFATTNINGSTAB.AV-LEVEL AT ROW 6.77 COL 20.25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY 1
          BGCOLOR 8 
     BEFATTNINGSTAB.KOSTNADSSLAG AT ROW 7.77 COL 20.25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
          BGCOLOR 8 
     BEFATTNINGSTAB.BILF AT ROW 8.77 COL 20.25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
          BGCOLOR 8 
     BEFATTNINGSTAB.PLUSD AT ROW 9.77 COL 20.25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
          BGCOLOR 8 
     BTN_OK AT ROW 12 COL 11.5
     BTN_AVBRYT AT ROW 12 COL 25
     RECT-12 AT ROW 11 COL 2
     RECT-11 AT ROW 1.09 COL 2
     SPACE(3.24) SKIP(3.54)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Befattningstab":L.


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

/* SETTINGS FOR FILL-IN BEFATTNINGSTAB.NAMN IN FRAME DIALOG-3
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-3
/* Query rebuild information for DIALOG-BOX DIALOG-3
     _TblList          = "RT8.BEFATTNINGSTAB"
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
    FIND FIRST BEFATTNINGSTAB WHERE RECID(BEFATTNINGSTAB) = brec EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE BEFATTNINGSTAB THEN DELETE BEFATTNINGSTAB.
  END.   
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-3
ON CHOOSE OF BTN_OK IN FRAME DIALOG-3 /* OK */
DO:
  {muswait.i}
  IF INPUT BEFATTNINGSTAB.BEFATTNING = "" THEN DO:
    MESSAGE "Befattning får ej vara blankt".
    RETURN NO-APPLY.
  END.  
  DO TRANSACTION:                            
    /*
    IF gamlabef ne "" THEN DO:
    
       FOR EACH PERSONALTAB WHERE PERSONALTAB.BEFATTNING = gamlabef:
          ASSIGN PERSONALTAB.BEFATTNING = INPUT BEFATTNINGSTAB.BEFATTNING.
       END.
       FOR EACH PERSONALTAB WHERE PERSONALTAB.BEFATTNING = BEFATTNINGSTAB.BEFATTNING:
          ASSIGN PERSONALTAB.PERSMASK = INPUT BEFATTNINGSTAB.PERSMASK
          PERSONALTAB.BRAVO = INPUT BEFATTNINGSTAB.BRAVO.
       END.     
       FOR EACH KUNDBEF WHERE KUNDBEF.BEFATTNING = gamlabef:
          ASSIGN KUNDBEF.BEFATTNING = INPUT BEFATTNINGSTAB.BEFATTNING.
       END. 
       FOR EACH KUNDOVER WHERE KUNDOVER.BEFATTNING = gamlabef:
          ASSIGN KUNDOVER.BEFATTNING = INPUT BEFATTNINGSTAB.BEFATTNING.
       END. 
       FOR EACH FAKTBEF WHERE FAKTBEF.BEFATTNING = gamlabef:
          ASSIGN FAKTBEF.BEFATTNING = INPUT BEFATTNINGSTAB.BEFATTNING.
       END. 
       FOR EACH FAKTOVER WHERE FAKTOVER.BEFATTNING = gamlabef:
          ASSIGN FAKTOVER.BEFATTNING = INPUT BEFATTNINGSTAB.BEFATTNING.
       END.
    END. 
    */              
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
     CREATE BEFATTNINGSTAB.
     ASSIGN FRAME {&FRAME-NAME}:TITLE = "NY".
     brec = RECID(BEFATTNINGSTAB).
     gamlabef = "".
   END.
   ELSE DO TRANSACTION:
     FIND BEFATTNINGSTAB WHERE RECID(BEFATTNINGSTAB) = BESTRECO EXCLUSIVE-LOCK.
     ASSIGN FRAME {&FRAME-NAME}:TITLE = "ÄNDRA-" + BEFATTNINGSTAB.BEFATTNING.
     brec = RECID(BEFATTNINGSTAB).  
     gamlabef = BEFATTNINGSTAB.BEFATTNING.
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
  IF AVAILABLE BEFATTNINGSTAB THEN 
    DISPLAY BEFATTNINGSTAB.BEFATTNING BEFATTNINGSTAB.NAMN BEFATTNINGSTAB.PRISA 
          BEFATTNINGSTAB.PERSMASK BEFATTNINGSTAB.BRAVO BEFATTNINGSTAB.AV-LEVEL 
          BEFATTNINGSTAB.KOSTNADSSLAG BEFATTNINGSTAB.BILF BEFATTNINGSTAB.PLUSD 
      WITH FRAME DIALOG-3.
  ENABLE RECT-12 RECT-11 BEFATTNINGSTAB.BEFATTNING BEFATTNINGSTAB.NAMN 
         BEFATTNINGSTAB.PRISA BEFATTNINGSTAB.PERSMASK BEFATTNINGSTAB.BRAVO 
         BEFATTNINGSTAB.AV-LEVEL BEFATTNINGSTAB.KOSTNADSSLAG 
         BEFATTNINGSTAB.BILF BEFATTNINGSTAB.PLUSD BTN_OK BTN_AVBRYT 
      WITH FRAME DIALOG-3.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-3}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


