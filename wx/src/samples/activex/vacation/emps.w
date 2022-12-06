&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Employees
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Employees 
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

DEFINE INPUT PARAMETER schedOCX AS COM-HANDLE.

/* Local Variable Definitions ---                                       */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Employees

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS SELECT-EMPLOYEES Btn_OK BUTTON-NEW 
&Scoped-Define DISPLAYED-OBJECTS SELECT-EMPLOYEES 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetEmployeeSelection Dialog-Employees 
FUNCTION GetEmployeeSelection RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Done" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-CHANGE 
     LABEL "Change" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-DELETE 
     LABEL "Delete" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-NEW 
     LABEL "New" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE SELECT-EMPLOYEES AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 41 BY 10.24 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Employees
     SELECT-EMPLOYEES AT ROW 1.48 COL 4 NO-LABEL
     Btn_OK AT ROW 1.52 COL 49
     BUTTON-NEW AT ROW 2.91 COL 49
     BUTTON-CHANGE AT ROW 4.33 COL 49
     BUTTON-DELETE AT ROW 5.76 COL 49
     SPACE(1.39) SKIP(5.52)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Employees"
         DEFAULT-BUTTON Btn_OK.


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
/* SETTINGS FOR DIALOG-BOX Dialog-Employees
                                                                        */
ASSIGN 
       FRAME Dialog-Employees:SCROLLABLE       = FALSE
       FRAME Dialog-Employees:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-CHANGE IN FRAME Dialog-Employees
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-DELETE IN FRAME Dialog-Employees
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Employees
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Employees Dialog-Employees
ON ENTRY OF FRAME Dialog-Employees /* Employees */
DO:
    RUN FillList.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Employees Dialog-Employees
ON WINDOW-CLOSE OF FRAME Dialog-Employees /* Employees */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-CHANGE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-CHANGE Dialog-Employees
ON CHOOSE OF BUTTON-CHANGE IN FRAME Dialog-Employees /* Change */
DO:
  DEFINE VARIABLE emp AS INTEGER.
  
  emp = GetEmployeeSelection().
  RUN empinfo.w PERSISTENT ( schedOCX, emp - 1).
  RUN FillList.
  select-employees:SCREEN-VALUE = select-employees:ENTRY( emp ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-DELETE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-DELETE Dialog-Employees
ON CHOOSE OF BUTTON-DELETE IN FRAME Dialog-Employees /* Delete */
DO:
    DEFINE VARIABLE emp-selected AS INTEGER.
    DEFINE VARIABLE new-selection AS INTEGER.
    
    emp-selected = GetEmployeeSelection().
    schedOCX:ResDelete(emp-selected - 1) = 1. /* ocx is 0 indexed */
    schedOCX:ResMax = schedOCX:ResMax - 1.
    select-employees:DELETE(emp-selected).
  
    IF select-employees:NUM-ITEMS >= emp-selected THEN
        new-selection = emp-selected.
    ELSE
        new-selection = select-employees:NUM-ITEMS.

    select-employees:SCREEN-VALUE = select-employees:ENTRY( new-selection).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-NEW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-NEW Dialog-Employees
ON CHOOSE OF BUTTON-NEW IN FRAME Dialog-Employees /* New */
DO:
    RUN empinfo.w PERSISTENT ( schedOCX, -1 ).
    RUN FillList.

    /* Select the one just added */
    select-employees:SCREEN-VALUE = 
        select-employees:ENTRY( select-employees:NUM-ITEMS ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SELECT-EMPLOYEES
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SELECT-EMPLOYEES Dialog-Employees
ON MOUSE-SELECT-DBLCLICK OF SELECT-EMPLOYEES IN FRAME Dialog-Employees
DO:
    DEFINE VARIABLE iEmp AS INTEGER.
    
    iEmp = GetEmployeeSelection().
    RUN empinfo.w PERSISTENT (schedOCX, iEmp - 1 ).
    RUN FillList.
    select-employees:SCREEN-VALUE = select-employees:ENTRY( iEmp ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SELECT-EMPLOYEES Dialog-Employees
ON VALUE-CHANGED OF SELECT-EMPLOYEES IN FRAME Dialog-Employees
DO:
    IF GetEmployeeSelection() > 0 THEN DO:
        button-change:SENSITIVE = TRUE.
        button-delete:SENSITIVE = TRUE.
    END.
    ELSE DO:
        button-change:SENSITIVE = FALSE.
        button-delete:SENSITIVE = FALSE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Employees 


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Employees _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Employees.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Employees _DEFAULT-ENABLE
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
  DISPLAY SELECT-EMPLOYEES 
      WITH FRAME Dialog-Employees.
  ENABLE SELECT-EMPLOYEES Btn_OK BUTTON-NEW 
      WITH FRAME Dialog-Employees.
  VIEW FRAME Dialog-Employees.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Employees}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillList Dialog-Employees 
PROCEDURE FillList :
/*------------------------------------------------------------------------------
  Purpose:  Fills the SELECT-EMPLOYEES list 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE i AS INTEGER.
    DEFINE VARIABLE cEmps AS INTEGER.
  
    cEmps = select-employees:NUM-ITEMS IN FRAME dialog-employees.
    REPEAT i = 1 TO cEmps:
        select-employees:DELETE( 1 ) IN FRAME dialog-employees.
    END.
   
    REPEAT i = 0 TO schedOCX:ResMax - 1: 
        select-employees:ADD-LAST( schedOCX:ResName(i) ) IN FRAME dialog-employees.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetEmployeeSelection Dialog-Employees 
FUNCTION GetEmployeeSelection RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Gets the selected employee from the employee list
    Notes:  
------------------------------------------------------------------------------*/

    DEFINE VARIABLE emp-selected AS CHARACTER.
    DEFINE VARIABLE emp-index    AS INTEGER.
  
    emp-selected = select-employees:SCREEN-VALUE IN FRAME dialog-employees.  
    emp-index = select-employees:LOOKUP( emp-selected ).

    RETURN emp-index.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


