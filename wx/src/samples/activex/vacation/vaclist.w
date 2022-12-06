&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Vacation-List
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Vacation-List 
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
&Scoped-define FRAME-NAME Dialog-Vacation-List

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_OK SELECT-EMPLOYEES SELECT-VACATIONS 
&Scoped-Define DISPLAYED-OBJECTS SELECT-EMPLOYEES SELECT-VACATIONS 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetEmployeeSelection Dialog-Vacation-List 
FUNCTION GetEmployeeSelection RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetVacationSelection Dialog-Vacation-List 
FUNCTION GetVacationSelection RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TBFromVac Dialog-Vacation-List 
FUNCTION TBFromVac RETURNS INTEGER
  ( input iCurEmp as integer, input iCurVac as integer )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "Done" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-CHANGE-VACATION 
     LABEL "Change" 
     SIZE 15 BY 1.14 TOOLTIP "Change Vacation".

DEFINE BUTTON BUTTON-DELETE-VACATION 
     LABEL "Delete" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-NEW-VACATION 
     LABEL "New" 
     SIZE 15 BY 1.14 TOOLTIP "New Vacation".

DEFINE VARIABLE SELECT-EMPLOYEES AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 33 BY 9.29 NO-UNDO.

DEFINE VARIABLE SELECT-VACATIONS AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 42 BY 9.29 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Vacation-List
     Btn_OK AT ROW 1.48 COL 83.2
     SELECT-EMPLOYEES AT ROW 2.43 COL 3 NO-LABEL
     SELECT-VACATIONS AT ROW 2.48 COL 39 NO-LABEL
     BUTTON-NEW-VACATION AT ROW 3.14 COL 83
     BUTTON-CHANGE-VACATION AT ROW 4.57 COL 83
     BUTTON-DELETE-VACATION AT ROW 6 COL 83
     "Employees:" VIEW-AS TEXT
          SIZE 20 BY 1 AT ROW 1.24 COL 3
     "Vacations:" VIEW-AS TEXT
          SIZE 19 BY 1 AT ROW 1.29 COL 39
     SPACE(41.79) SKIP(9.99)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Vacation List"
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
/* SETTINGS FOR DIALOG-BOX Dialog-Vacation-List
                                                                        */
ASSIGN 
       FRAME Dialog-Vacation-List:SCROLLABLE       = FALSE
       FRAME Dialog-Vacation-List:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON BUTTON-CHANGE-VACATION IN FRAME Dialog-Vacation-List
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-DELETE-VACATION IN FRAME Dialog-Vacation-List
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON BUTTON-NEW-VACATION IN FRAME Dialog-Vacation-List
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Vacation-List
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Vacation-List Dialog-Vacation-List
ON ENTRY OF FRAME Dialog-Vacation-List /* Vacation List */
DO:
  RUN FillEmployeeList.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Vacation-List Dialog-Vacation-List
ON WINDOW-CLOSE OF FRAME Dialog-Vacation-List /* Vacation List */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-CHANGE-VACATION
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-CHANGE-VACATION Dialog-Vacation-List
ON CHOOSE OF BUTTON-CHANGE-VACATION IN FRAME Dialog-Vacation-List /* Change */
DO:
    DEFINE VARIABLE iEmp AS INTEGER.
    DEFINE VARIABLE iVac AS INTEGER.
    DEFINE VARIABLE iTB  AS INTEGER.
    
    iEmp = GetEmployeeSelection() - 1.
    iVac = GetVacationSelection() -  1.
    iTB = TBFromVac( iEmp, iVac ).
    
    RUN vacinfo.w PERSISTENT ( 
        schedOCX, 
        iEmp,
        iTB ).
        
    RUN FillVacationList( iEmp ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-DELETE-VACATION
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-DELETE-VACATION Dialog-Vacation-List
ON CHOOSE OF BUTTON-DELETE-VACATION IN FRAME Dialog-Vacation-List /* Delete */
DO:
    DEFINE VARIABLE emp-selected AS INTEGER.
    DEFINE VARIABLE vac-selected AS INTEGER.
    DEFINE VARIABLE i            AS INTEGER.
    DEFINE VARIABLE iTBFound     AS INTEGER.
  
    emp-selected = GetEmployeeSelection() - 1.
    vac-selected = GetVacationSelection() - 1.
    iTBFound = -1.
 
    REPEAT i = 0 TO schedOCX:TBMax - 1:
        IF schedOCX:TBResource(i) = emp-selected THEN DO:
            iTBFound = iTBFound + 1.
            IF iTBFound = vac-selected THEN
                LEAVE.
        END.
    END.
  
  schedOCX:TBDelete(i) = 1. /* ocx is 0 indexed */
  schedOCX:TBMax = schedOCX:TBMax - 1.
  select-vacations:DELETE(vac-selected + 1).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-NEW-VACATION
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-NEW-VACATION Dialog-Vacation-List
ON CHOOSE OF BUTTON-NEW-VACATION IN FRAME Dialog-Vacation-List /* New */
DO:
    DEFINE VARIABLE iEmp AS INTEGER.
    
    iEmp = GetEmployeeSelection() - 1.
    
    RUN vacinfo.w PERSISTENT ( 
        schedOCX, 
        iEmp,
        -1 ).
        
    RUN FillVacationList( iEmp ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SELECT-EMPLOYEES
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SELECT-EMPLOYEES Dialog-Vacation-List
ON VALUE-CHANGED OF SELECT-EMPLOYEES IN FRAME Dialog-Vacation-List
DO:
    DEFINE VARIABLE iSel   AS INTEGER.
    DEFINE VARIABLE i      AS INTEGER.
    DEFINE VARIABLE cItems AS INTEGER.
  
    iSel = GetEmployeeSelection() - 1.
  
    RUN FillVacationList( iSel ).
 
    IF GetEmployeeSelection() > 0 THEN
        button-new-vacation:SENSITIVE = TRUE.
    ELSE
        button-new-vacation:SENSITIVE = FALSE.
    
    IF select-vacations:NUM-ITEMS > 0 THEN
        select-vacations:SCREEN-VALUE = select-vacations:ENTRY( 1 ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SELECT-VACATIONS
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SELECT-VACATIONS Dialog-Vacation-List
ON MOUSE-SELECT-DBLCLICK OF SELECT-VACATIONS IN FRAME Dialog-Vacation-List
DO:
    DEFINE VARIABLE iEmp AS INTEGER.
    DEFINE VARIABLE iTB  AS INTEGER.
    
    iEmp = GetEmployeeSelection() - 1.
    iTB = TBFromVac( iEmp, GetVacationSelection() - 1 ).
    
    RUN vacinfo.w PERSISTENT ( 
        schedOCX, 
        iEmp,
        iTB ).

    RUN FillVacationList( iEmp ).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SELECT-VACATIONS Dialog-Vacation-List
ON VALUE-CHANGED OF SELECT-VACATIONS IN FRAME Dialog-Vacation-List
DO:
    IF GetVacationSelection() > 0 THEN DO:
        button-change-vacation:SENSITIVE = TRUE.
        button-delete-vacation:SENSITIVE = TRUE.
    END.
    ELSE DO:
        button-change-vacation:SENSITIVE = FALSE.
        button-delete-vacation:SENSITIVE = FALSE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Vacation-List 


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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Vacation-List _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Vacation-List.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Vacation-List _DEFAULT-ENABLE
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
  DISPLAY SELECT-EMPLOYEES SELECT-VACATIONS 
      WITH FRAME Dialog-Vacation-List.
  ENABLE Btn_OK SELECT-EMPLOYEES SELECT-VACATIONS 
      WITH FRAME Dialog-Vacation-List.
  VIEW FRAME Dialog-Vacation-List.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Vacation-List}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillEmployeeList Dialog-Vacation-List 
PROCEDURE FillEmployeeList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE VARIABLE i AS INTEGER.
  DEFINE VARIABLE cEmps AS INTEGER.
  
  cEmps = select-employees:NUM-ITEMS IN FRAME dialog-vacation-list.
    
  REPEAT i = 1 TO cEmps:
    select-employees:DELETE(1) IN FRAME dialog-vacation-list.
  END.
  
  REPEAT i = 0 TO schedOCX:ResMax - 1: 
    select-employees:ADD-LAST( schedOCX:ResName(i) ) IN FRAME dialog-vacation-list.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FillVacationList Dialog-Vacation-List 
PROCEDURE FillVacationList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER iEmp AS INTEGER.
 
  DEFINE VARIABLE i     AS INTEGER.
  DEFINE VARIABLE cVacs AS INTEGER.
  DEFINE VARIABLE s     AS CHARACTER.  
  
  cVacs = select-vacations:NUM-ITEMS IN FRAME dialog-vacation-list.
    
  REPEAT i = 1 TO cVacs:
    select-vacations:DELETE(1) IN FRAME dialog-vacation-list.
  END.
  
  REPEAT i = 0 TO schedOCX:TBMax - 1:
    IF schedOCX:TBResource(i) = iEmp THEN DO:
      s = schedOCX:TBUser2(i).
      s = s + " From:" + schedOCX:TBBeg(i).
      s = s + " to:" + schedOCX:TBEnd(i).
        
      select-vacations:ADD-LAST( s ).
    END.
  END.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetEmployeeSelection Dialog-Vacation-List 
FUNCTION GetEmployeeSelection RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  returns index of selected employee
    Notes:  1 indexed
------------------------------------------------------------------------------*/

  DEFINE VARIABLE emp-selected AS CHARACTER.
  DEFINE VARIABLE emp-index    AS INTEGER.
  
  emp-selected = select-employees:SCREEN-VALUE IN FRAME dialog-vacation-list.  
  emp-index = select-employees:LOOKUP( emp-selected ).

  RETURN emp-index.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetVacationSelection Dialog-Vacation-List 
FUNCTION GetVacationSelection RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Gets the selected vacation from the vacation list
    Notes:  
------------------------------------------------------------------------------*/

  DEFINE VARIABLE vac-selected AS CHARACTER.
  DEFINE VARIABLE vac-index    AS INTEGER.
  
  vac-selected = select-vacations:SCREEN-VALUE IN FRAME dialog-vacation-list.  
  vac-index = select-vacations:LOOKUP( vac-selected ).

  RETURN vac-index.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TBFromVac Dialog-Vacation-List 
FUNCTION TBFromVac RETURNS INTEGER
  ( input iCurEmp as integer, input iCurVac as integer ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iTB    AS INTEGER.
  DEFINE VARIABLE iFound AS INTEGER.
  
  iFound = -1.
  
  REPEAT iTB = 0 TO schedOCX:TBMax - 1:
    IF schedOCX:TBResource(iTB) = iCurEmp  THEN DO:
        iFound = iFound + 1.
        IF iFound = iCurVac THEN
            LEAVE.
    END.
  END.
  
  RETURN iTB.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


