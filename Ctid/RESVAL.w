&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME DIALOG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS DIALOG-1 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 10/15/96 -  2:40 pm

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER varifran AS INTEGER NO-UNDO.
/* Local Variable Definitions ---                                       */
{ALLDEF.I}

&Scoped-define NEW
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DIALOG-1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RAD_MKTID BTN_OK 
&Scoped-Define DISPLAYED-OBJECTS RAD_MKTID 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN_OK 
     LABEL "Ok" 
     SIZE 14 BY 1.

DEFINE VARIABLE RAD_MKTID AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Endags- eller flerdygnsf?r?ttning", 1,
"?ndra tj?nsteresa", 3,
"Utlandsresa", 4,
"Radera tj?nsteresa", 2
     SIZE 36 BY 4.17 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DIALOG-1
     RAD_MKTID AT ROW 3 COL 1.5 NO-LABEL
     BTN_OK AT ROW 7.58 COL 23.5
     "Tj?nsteresan skall g?lla:" VIEW-AS TEXT
          SIZE 34.5 BY 1.75 AT ROW 1.25 COL 1.5
          FONT 17
     SPACE(2.24) SKIP(5.78)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Tj?nsteresor".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX DIALOG-1
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME DIALOG-1:SCROLLABLE       = FALSE
       FRAME DIALOG-1:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX DIALOG-1
/* Query rebuild information for DIALOG-BOX DIALOG-1
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX DIALOG-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BTN_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN_OK DIALOG-1
ON CHOOSE OF BTN_OK IN FRAME DIALOG-1 /* Ok */
DO:
   {muswait.i}  
   RAD_MKTID = INPUT RAD_MKTID.
   varifran = RAD_MKTID.
   APPLY "GO" TO FRAME {&FRAME-NAME}.
   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RAD_MKTID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RAD_MKTID DIALOG-1
ON VALUE-CHANGED OF RAD_MKTID IN FRAME DIALOG-1
DO:
   RAD_MKTID = INPUT RAD_MKTID.
   varifran = RAD_MKTID.
   {muswait.i} 
   APPLY "GO" TO FRAME {&FRAME-NAME}. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK DIALOG-1 


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
   {musarrow.i}  
   RUN enable_UI.       
   {FRMSIZED.I}
   IF Guru.Konstanter:tidtsekvar[1] = FALSE THEN DO: 
      status-ok = RAD_MKTID:DELETE("Endags- eller flerdygnsf?r?ttning"). 
   END.
   IF Guru.Konstanter:tidtsekvar[2] = FALSE  THEN DO:
      status-ok = RAD_MKTID:DELETE("Radera tj?nsteresa"). 
   END.
   IF Guru.Konstanter:tidtsekvar[5] = FALSE THEN DO:
      status-ok = RAD_MKTID:DELETE("?ndra tj?nsteresa"). 
   END.
   ELSE DO:
      /*ingen ska ha ?ndra tj?nsteresa*/
      status-ok = RAD_MKTID:DELETE("?ndra tj?nsteresa"). 
   END.
   IF Guru.Konstanter:tidtsekvar[6] = FALSE THEN DO:
      status-ok = RAD_MKTID:DELETE("Registrera utlandsresa"). 
   END.
   
   {DIA_M_SLUT.I}
   WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI DIALOG-1  _DEFAULT-DISABLE
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
  HIDE FRAME DIALOG-1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI DIALOG-1  _DEFAULT-ENABLE
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
  DISPLAY RAD_MKTID 
      WITH FRAME DIALOG-1.
  ENABLE RAD_MKTID BTN_OK 
      WITH FRAME DIALOG-1.
  {&OPEN-BROWSERS-IN-QUERY-DIALOG-1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

