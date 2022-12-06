&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Employee-Info
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Employee-Info 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

DEFINE INPUT PARAMETER schedOCX AS COM-HANDLE NO-UNDO.
DEFINE INPUT PARAMETER iEmp     AS INTEGER NO-UNDO.

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE emp-index AS INTEGER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Employee-Info

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FILL-IN-EMP-NAME Btn_OK COMBO-BOX-EMP-TYPE ~
Btn_Cancel 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-EMP-NAME COMBO-BOX-EMP-TYPE 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Cancel" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE COMBO-BOX-EMP-TYPE AS CHARACTER FORMAT "X(256)":U 
     LABEL "Employee Type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS " "
     SIZE 23 BY 1 TOOLTIP "Employee Type" NO-UNDO.

DEFINE VARIABLE FILL-IN-EMP-NAME AS CHARACTER FORMAT "X(256)":U 
     LABEL "Employee Name" 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 TOOLTIP "Employee Name" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Employee-Info
     FILL-IN-EMP-NAME AT ROW 1.71 COL 18 COLON-ALIGNED
     Btn_OK AT ROW 1.71 COL 48
     COMBO-BOX-EMP-TYPE AT ROW 3.14 COL 18 COLON-ALIGNED
     Btn_Cancel AT ROW 3.14 COL 48
     SPACE(3.19) SKIP(1.04)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Employee Information"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Employee-Info
                                                                        */
ASSIGN 
       FRAME Employee-Info:SCROLLABLE       = FALSE
       FRAME Employee-Info:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Employee-Info
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Employee-Info Employee-Info
ON ENTRY OF FRAME Employee-Info /* Employee Information */
DO:
    DEFINE VARIABLE emp-selected AS CHARACTER.
    DEFINE VARIABLE i            AS INTEGER.
    DEFINE VARIABLE rbd          AS INTEGER.    
    
    REPEAT i = 0 TO schedOCX:BarsMax - 1:
        combo-box-emp-type:ADD-LAST( schedOCX:BarName(i)).        
    END.

    /* If this dialog is brought up for change, then show the current
       bar type for this employee. 
     */
    IF iEmp >= 0 THEN DO:
        fill-in-emp-name:SCREEN-VALUE = schedOCX:ResName(iEmp).
        rbd = schedOCX:ResBarDefault( iEmp ) + 1. 
        combo-box-emp-type:SCREEN-VALUE = combo-box-emp-type:ENTRY(rbd).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Employee-Info Employee-Info
ON WINDOW-CLOSE OF FRAME Employee-Info /* Employee Information */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Employee-Info
ON CHOOSE OF Btn_OK IN FRAME Employee-Info /* OK */
DO:
  DEFINE VARIABLE iChangedEmp AS INTEGER.
    
  IF iEmp < 0 AND LENGTH(fill-in-emp-name:screen-value) > 0 THEN DO:
    schedOCX:ResMax = schedOCX:ResMax + 1.
    iChangedEmp = schedOCX:ResMax - 1.
  END.
  ELSE DO:
    iChangedEmp = iEmp.
  END.
  
  IF LENGTH( fill-in-emp-name:screen-value ) > 0 THEN DO:
    schedOCX:ResName( iChangedEmp ) = fill-in-emp-name:SCREEN-VALUE.
    schedOCX:ResBarDefault( iChangedEmp ) = 
        combo-box-emp-type:LOOKUP(combo-box-emp-type:SCREEN-VALUE) - 1.
    schedOCX:ResLineHeight( iChangedEmp ) = 28.
    schedOCX:ResFontSizePercent( iChangedEmp ) = 75.
  END.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Employee-Info 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Employee-Info _DEFAULT-DISABLE
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
  HIDE FRAME Employee-Info.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Employee-Info _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-EMP-NAME COMBO-BOX-EMP-TYPE 
      WITH FRAME Employee-Info.
  ENABLE FILL-IN-EMP-NAME Btn_OK COMBO-BOX-EMP-TYPE Btn_Cancel 
      WITH FRAME Employee-Info.
  VIEW FRAME Employee-Info.
  {&OPEN-BROWSERS-IN-QUERY-Employee-Info}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


