&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME Vacation-Window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Vacation-Window 
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

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE DisplayMode_ResView     AS INTEGER INITIAL 0.
DEFINE VARIABLE DisplayMode_YearlyHorz  AS INTEGER INITIAL 1.
DEFINE VARIABLE DisplayMode_YearlyVert  AS INTEGER INITIAL 2.
DEFINE VARIABLE DisplayMode_MonthlyHorz AS INTEGER INITIAL 3.
DEFINE VARIABLE DisplayMode_MonthlyVert AS INTEGER INITIAL 4.
DEFINE VARIABLE DisplayMode_WeeklyHorz  AS INTEGER INITIAL 5.
DEFINE VARIABLE DisplayMode_WeeklyVert  AS INTEGER INITIAL 6.
DEFINE VARIABLE DisplayMode_DailyHorz   AS INTEGER INITIAL 7.
DEFINE VARIABLE DisplayMode_DailyVert   AS INTEGER INITIAL 8.
DEFINE VARIABLE DisplayMode_CalHorz     AS INTEGER INITIAL 9.
DEFINE VARIABLE DisplayMode_CalVert     AS INTEGER INITIAL 10.

DEFINE VARIABLE TimeScale_Year          AS INTEGER INITIAL 0.
DEFINE VARIABLE TimeScale_Quarter       AS INTEGER INITIAL 1.
DEFINE VARIABLE TimeScale_Month         AS INTEGER INITIAL 2.
DEFINE VARIABLE TimeScale_Week          AS INTEGER INITIAL 3.
DEFINE VARIABLE TimeScale_Day           AS INTEGER INITIAL 4.
DEFINE VARIABLE TimeScale_Hour          AS INTEGER INITIAL 5.
DEFINE VARIABLE TimeScale_Minute        AS INTEGER INITIAL 6.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME VACATION-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-YEAR BUTTON-MONTH BUTTON-WEEK ~
BUTTON-EMPLOYEE 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD HControl Vacation-Window 
FUNCTION HControl RETURNS COM-HANDLE
    ( input hFrame as WIDGET-HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR Vacation-Window AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_Edit 
       MENU-ITEM m_Employees    LABEL "Employees"     
       MENU-ITEM m_Vacations    LABEL "Vacations"     .

DEFINE MENU MENU-BAR-Vacation-Window MENUBAR
       MENU-ITEM m_Quit         LABEL "Quit!"         
       SUB-MENU  m_Edit         LABEL "Edit"          .


/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE schedOCXFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chschedOCXFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-EMPLOYEE 
     IMAGE-UP FILE "people":U
     LABEL "Employee" 
     SIZE 7 BY 1.67 TOOLTIP "Employee View".

DEFINE BUTTON BUTTON-MONTH 
     IMAGE-UP FILE "monthly":U
     LABEL "Month" 
     SIZE 7 BY 1.67 TOOLTIP "Month View".

DEFINE BUTTON BUTTON-WEEK 
     IMAGE-UP FILE "weekly":U
     LABEL "Week" 
     SIZE 7 BY 1.67 TOOLTIP "Week View".

DEFINE BUTTON BUTTON-YEAR 
     IMAGE-UP FILE "yearly":U
     LABEL "Year" 
     SIZE 7 BY 1.67 TOOLTIP "Year View".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME VACATION-FRAME
     BUTTON-YEAR AT ROW 1.24 COL 1
     BUTTON-MONTH AT ROW 1.24 COL 8
     BUTTON-WEEK AT ROW 1.24 COL 15
     BUTTON-EMPLOYEE AT ROW 1.24 COL 22
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 151.8 BY 23.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW Vacation-Window ASSIGN
         HIDDEN             = YES
         TITLE              = "Vacation Schedule"
         COLUMN             = 25.6
         ROW                = 7.43
         HEIGHT             = 23
         WIDTH              = 151.8
         MAX-HEIGHT         = 23
         MAX-WIDTH          = 151.8
         VIRTUAL-HEIGHT     = 23
         VIRTUAL-WIDTH      = 151.8
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-Vacation-Window:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW Vacation-Window
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME VACATION-FRAME
                                                                        */
ASSIGN 
       FRAME VACATION-FRAME:HEIGHT           = 23
       FRAME VACATION-FRAME:WIDTH            = 151.8.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(Vacation-Window)
THEN Vacation-Window:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME schedOCXFrame ASSIGN
       FRAME        = FRAME VACATION-FRAME:HANDLE
       ROW          = 3.14
       COLUMN       = 1
       HEIGHT       = 20.86
       WIDTH        = 151.8
       HIDDEN       = no
       SENSITIVE    = yes.
      schedOCXFrame:NAME = "schedOCXFrame":U .
/* schedOCXFrame OCXINFO:CREATE-CONTROL from: {CD01D844-3472-11CF-99E4-00006E22DD91} type: SchedOCX */
      schedOCXFrame:MOVE-AFTER(BUTTON-EMPLOYEE:HANDLE IN FRAME VACATION-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Vacation-Window
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Vacation-Window Vacation-Window
ON END-ERROR OF Vacation-Window /* Vacation Schedule */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Vacation-Window Vacation-Window
ON WINDOW-CLOSE OF Vacation-Window /* Vacation Schedule */
DO:
  RUN SaveAndQuit.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VACATION-FRAME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VACATION-FRAME Vacation-Window
ON ENTRY OF FRAME VACATION-FRAME
DO:
  DEFINE VARIABLE schedOCX AS COM-HANDLE.
  
  schedOCX = HControl( schedOCXFrame ).
  
  schedOCX:ResLoad = "VACRES.TXT".
  schedOCX:TBLoad = "VACTB.TXT".
  
  schedOCX:DefaultDateTimeFormat = "INTLSHORTDATE".
  
  RELEASE OBJECT schedOCX.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-EMPLOYEE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-EMPLOYEE Vacation-Window
ON CHOOSE OF BUTTON-EMPLOYEE IN FRAME VACATION-FRAME /* Employee */
DO:
  DEFINE VARIABLE schedOCX AS COM-HANDLE.
  
  schedOCX = HControl( schedOCXFrame ).
  
  schedOCX:TSMinor(0) = TimeScale_Day.
  schedOCX:DisplayMode = DisplayMode_ResView.
  
  RELEASE OBJECT schedOCX.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-MONTH
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-MONTH Vacation-Window
ON CHOOSE OF BUTTON-MONTH IN FRAME VACATION-FRAME /* Month */
DO:
  DEFINE VARIABLE schedOCX AS COM-HANDLE.
  
  schedOCX = HControl( schedOCXFrame ).
  
  schedOCX:DisplayMode = DisplayMode_MonthlyHorz.
  RELEASE OBJECT schedOCX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-WEEK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-WEEK Vacation-Window
ON CHOOSE OF BUTTON-WEEK IN FRAME VACATION-FRAME /* Week */
DO:
  DEFINE VARIABLE schedOCX AS COM-HANDLE.
  
  schedOCX = HControl( schedOCXFrame ).
  
  schedOCX:TSMinor(0) = TimeScale_Year.
  schedOCX:DisplayMode = DisplayMode_WeeklyHorz.
  
  RELEASE OBJECT schedOCX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-YEAR
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-YEAR Vacation-Window
ON CHOOSE OF BUTTON-YEAR IN FRAME VACATION-FRAME /* Year */
DO:
  DEFINE VARIABLE schedOCX AS COM-HANDLE.
  schedOCX = HControl( schedOCXFrame ).
  
  schedOCX:DisplayMode = DisplayMode_YearlyHorz.
  
  RELEASE OBJECT schedOCX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Employees
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Employees Vacation-Window
ON CHOOSE OF MENU-ITEM m_Employees /* Employees */
DO:
   DEFINE VARIABLE schedOCX AS COM-HANDLE.

   schedOCX = HControl( schedOCXFrame ).
   RUN emps.w PERSISTENT ( schedOCX ).
   
   /* Do not release schedOCX or it will invalidate the
      parameter received by emps.w
    */       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Quit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Quit Vacation-Window
ON CHOOSE OF MENU-ITEM m_Quit /* Quit! */
DO:
  RUN SaveAndQuit.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Vacations
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Vacations Vacation-Window
ON CHOOSE OF MENU-ITEM m_Vacations /* Vacations */
DO:
  DEFINE VARIABLE schedOCX AS COM-HANDLE.
  schedOCX = HControl( schedOCXFrame ).
   
  RUN vaclist.w PERSISTENT ( schedOCX ).
  
  /* Do not release schedOCX or it will invalidate the
     parameter received by vaclist.w
   */     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME schedOCXFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL schedOCXFrame Vacation-Window
PROCEDURE schedOCXFrame.SchedOCX.BarDoubleClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    TimeBlock
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-TimeBlock AS INTEGER NO-UNDO.

    DEFINE VARIABLE schedOCX AS COM-HANDLE.
    DEFINE VARIABLE iEmp AS INTEGER.
    
    schedOCX = hControl( schedOCXFrame ).
    iEmp = schedOCX:TBResource( p-TimeBlock ).
    
    RUN vacinfo.w PERSISTENT ( schedOCX, iEmp, p-Timeblock ).
    
    /* Do not release schedOCX or it will invalidate the
      parameter received by vacinfo.w
    */     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL schedOCXFrame Vacation-Window
PROCEDURE schedOCXFrame.SchedOCX.ResHdrDoubleClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    Col
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-Col AS INTEGER NO-UNDO.
   
   RUN emps.w PERSISTENT ( COM-SELF ).
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL schedOCXFrame Vacation-Window
PROCEDURE schedOCXFrame.SchedOCX.TS0MinorDoubleClick .
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Required for OCX.
    DateTime
    TSMinor
  Notes:       
------------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER p-DateTime AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER p-TSMinor  AS INTEGER NO-UNDO.

   RUN vaclist.w PERSISTENT ( COM-SELF ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Vacation-Window 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load Vacation-Window _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "vacation.wrx":U ).

IF OCXFile <> ? THEN DO:
  ASSIGN
    chschedOCXFrame = schedOCXFrame:COM-HANDLE
    UIB_S = chschedOCXFrame:LoadControls( OCXFile, "schedOCXFrame":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "The file, vacation.wrx, could not be found." skip
             "The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Vacation-Window _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(Vacation-Window)
  THEN DELETE WIDGET Vacation-Window.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EmployeeList Vacation-Window 
PROCEDURE EmployeeList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE schedOCX AS COM-HANDLE.
 
  schedOCX = hControl( schedOCXFrame ).
  RUN emps.w PERSISTENT( schedOCX ).
 
  /* Do not release schedOCX or it will invalidate the
     parameter received by emps.w
   */      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Vacation-Window _DEFAULT-ENABLE
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
  RUN control_load.
  ENABLE BUTTON-YEAR BUTTON-MONTH BUTTON-WEEK BUTTON-EMPLOYEE 
      WITH FRAME VACATION-FRAME IN WINDOW Vacation-Window.
  {&OPEN-BROWSERS-IN-QUERY-VACATION-FRAME}
  VIEW Vacation-Window.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveAndQuit Vacation-Window 
PROCEDURE SaveAndQuit :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE schedOCX AS COM-HANDLE.
  
  schedOCX = HControl( schedOCXFrame ).
  
  schedOCX:ResSave = "VACRES.TXT".
  schedOCX:TBSave = "VACTB.TXT".
  
  RELEASE OBJECT schedOCX.
  
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION HControl Vacation-Window 
FUNCTION HControl RETURNS COM-HANDLE
    ( input hFrame as WIDGET-HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  gets control's com-handle from control frame
    Notes:  Function Implementation.
------------------------------------------------------------------------------*/

  DEFINE VARIABLE hColl    AS COM-HANDLE.
  DEFINE VARIABLE hControl AS COM-HANDLE.
  DEFINE VARIABLE hcFrame  AS COM-HANDLE.

  hcFrame = hFrame:COM-HANDLE.
  hColl = hcFrame:Controls.
  hControl = hColl:Item(1).
  RELEASE OBJECT hColl.
  RETURN hControl.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


