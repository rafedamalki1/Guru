&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME actuate_edit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS actuate_edit 
/*------------------------------------------------------------------------

  File: acedit.w

  Description: Example of how to :
   o read, edit and display Actuate Report parameters
   o save updated parameters to a new Actuate parameter file
   o generate a new Actuate report
   
  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: Gerry Seidl

  Created: July 1, 1997
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{samples/actuate/acapi.i} /* Actuate API */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE rc               AS INTEGER   NO-UNDO.
DEFINE VARIABLE fileNumber       AS INTEGER   NO-UNDO.
DEFINE VARIABLE currentGroup     AS CHARACTER NO-UNDO.
DEFINE VARIABLE currentParameter AS CHARACTER NO-UNDO.  

/* TEMP-TABLE DEFINITIONS */
DEFINE TEMP-TABLE rptparams 
  FIELD p_group    AS CHARACTER FORMAT "X(128)"
  FIELD p_name     AS CHARACTER FORMAT "X(128)"
  FIELD p_alias    AS CHARACTER FORMAT "X(128)"
  FIELD p_required AS LOGICAL
  FIELD p_hidden   AS LOGICAL
  FIELD p_HideText AS LOGICAL
  FIELD p_AdHoc    AS LOGICAL
  FIELD p_ParmType AS CHARACTER FORMAT "X(15)"
  FIELD p_value    AS CHARACTER
  INDEX groupname IS PRIMARY p_group p_name.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME actuate_edit

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS pc_filename btn_Browse Btn_OK RECT-2 ~
GroupName ParamName RECT-1 CurrentValue btn_SaveValue btn_writeROV ~
btn_generate 
&Scoped-Define DISPLAYED-OBJECTS pc_filename GroupName ParamName ParamAlias ~
ParamType tog_req tog_HideText tog_Hidden tog_AdHoc CurrentValue 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btn_Browse 
     LABEL "Browse..." 
     SIZE 11 BY 1.

DEFINE BUTTON btn_generate 
     LABEL "Generate new report" 
     SIZE 30 BY 1.14.

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btn_SaveValue 
     LABEL "Save Value" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btn_writeROV 
     LABEL "Create new .ROV file" 
     SIZE 24 BY 1.14.

DEFINE VARIABLE GroupName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Group" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     LIST-ITEMS " "
     SIZE 66 BY 1 NO-UNDO.

DEFINE VARIABLE ParamName AS CHARACTER FORMAT "X(256)":U 
     LABEL "Name" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     LIST-ITEMS " "
     SIZE 66 BY 1 NO-UNDO.

DEFINE VARIABLE CurrentValue AS CHARACTER FORMAT "X(256)":U 
     LABEL "Value" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE ParamAlias AS CHARACTER FORMAT "X(256)":U 
     LABEL "Alias" 
     VIEW-AS FILL-IN 
     SIZE 62 BY 1 NO-UNDO.

DEFINE VARIABLE ParamType AS CHARACTER FORMAT "X(256)":U 
     LABEL "Type" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE pc_filename AS CHARACTER FORMAT "X(256)":U 
     LABEL "Filename" 
     VIEW-AS FILL-IN 
     SIZE 54 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 76 BY 7.86.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 76 BY 3.14.

DEFINE VARIABLE tog_AdHoc AS LOGICAL INITIAL no 
     LABEL "Ad Hoc" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .81 NO-UNDO.

DEFINE VARIABLE tog_Hidden AS LOGICAL INITIAL no 
     LABEL "Hidden" 
     VIEW-AS TOGGLE-BOX
     SIZE 13 BY .81 NO-UNDO.

DEFINE VARIABLE tog_HideText AS LOGICAL INITIAL no 
     LABEL "Hide Text" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .81 NO-UNDO.

DEFINE VARIABLE tog_req AS LOGICAL INITIAL no 
     LABEL "Required" 
     VIEW-AS TOGGLE-BOX
     SIZE 14 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME actuate_edit
     pc_filename AT ROW 1.67 COL 11 COLON-ALIGNED
     btn_Browse AT ROW 1.67 COL 68
     Btn_OK AT ROW 1.71 COL 82
     GroupName AT ROW 3.62 COL 9 COLON-ALIGNED
     ParamName AT ROW 4.81 COL 9 COLON-ALIGNED
     ParamAlias AT ROW 7.19 COL 9 COLON-ALIGNED
     ParamType AT ROW 9.1 COL 9 COLON-ALIGNED
     tog_req AT ROW 9.1 COL 31
     tog_HideText AT ROW 9.1 COL 53
     tog_Hidden AT ROW 10.05 COL 31
     tog_AdHoc AT ROW 10.05 COL 53
     CurrentValue AT ROW 11.48 COL 9 COLON-ALIGNED
     btn_SaveValue AT ROW 12.91 COL 11
     btn_writeROV AT ROW 15.05 COL 13
     btn_generate AT ROW 15.05 COL 43
     "Parameters" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 2.91 COL 5
     RECT-2 AT ROW 3.1 COL 3
     "Parameter attributes" VIEW-AS TEXT
          SIZE 20 BY .62 AT ROW 6.48 COL 5
     RECT-1 AT ROW 6.71 COL 3
     SPACE(18.99) SKIP(1.90)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Edit Actuate Report Parameters"
         DEFAULT-BUTTON Btn_OK.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX actuate_edit
                                                                        */
ASSIGN 
       FRAME actuate_edit:SCROLLABLE       = FALSE
       FRAME actuate_edit:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN ParamAlias IN FRAME actuate_edit
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ParamType IN FRAME actuate_edit
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tog_AdHoc IN FRAME actuate_edit
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tog_Hidden IN FRAME actuate_edit
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tog_HideText IN FRAME actuate_edit
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX tog_req IN FRAME actuate_edit
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME actuate_edit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL actuate_edit actuate_edit
ON WINDOW-CLOSE OF FRAME actuate_edit /* Edit Actuate Report Parameters */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_Browse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_Browse actuate_edit
ON CHOOSE OF btn_Browse IN FRAME actuate_edit /* Browse... */
DO:
  DEFINE VAR l_ok              AS LOGICAL             NO-UNDO.
  DEFINE VAR filename          AS CHARACTER           NO-UNDO.
  DEFINE VAR Filter_NameString AS CHARACTER EXTENT 3  NO-UNDO.
  DEFINE VAR Filter_FileSpec   LIKE Filter_NameString NO-UNDO.

  /* Initialize the file filters */
  ASSIGN Filter_NameString[ 1 ] = "All parameter files (*.rov~;*.rox)"
         Filter_FileSpec[ 1 ]   = "*.rov~;*.rox"
         Filter_NameString[ 2 ] = "Report Executable (*.rox)"
         Filter_FileSpec[ 2 ]   = "*.rox"
         Filter_NameString[ 3 ] = "Report parameter values (*.rov)"
         Filter_FileSpec[ 3 ]   = "*.rov". 

  /* Ask for a file name. NOTE: File-names to run must exist */                          
  filename = pc_filename:SCREEN-VALUE.
  SYSTEM-DIALOG GET-FILE filename
      TITLE    "Open report"
      FILTERS  Filter_NameString[ 1 ]   Filter_FileSpec[ 1 ],
               Filter_NameString[ 2 ]   Filter_FileSpec[ 2 ],
               Filter_NameString[ 3 ]   Filter_FileSpec[ 3 ]
      MUST-EXIST
      UPDATE   l_ok IN WINDOW {&WINDOW-NAME}.  
  IF l_ok THEN DO:
    pc_filename:SCREEN-VALUE = filename.
    APPLY "RETURN":U TO pc_filename.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_generate
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_generate actuate_edit
ON CHOOSE OF btn_generate IN FRAME actuate_edit /* Generate new report */
DO:
  RUN samples/actuate/acgendlg.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_SaveValue
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_SaveValue actuate_edit
ON CHOOSE OF btn_SaveValue IN FRAME actuate_edit /* Save Value */
DO:
  ASSIGN CurrentValue.
  
  /* Save the new value back to the report parameter */
  CASE ParamType:
      WHEN "String":U   OR
      WHEN "Currency":U OR
      WHEN "Date":U     THEN RUN AcReqSetValueString  (INPUT fileNumber, INPUT ParamName, INPUT CurrentValue).
      WHEN "Integer":U  THEN RUN AcReqSetValueInteger (INPUT fileNumber, INPUT ParamName, INPUT INTEGER(CurrentValue)).
      WHEN "Double":U   THEN RUN AcReqSetValueDouble  (INPUT fileNumber, INPUT ParamName, INPUT DECIMAL(CurrentValue)).
  END CASE.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_writeROV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_writeROV actuate_edit
ON CHOOSE OF btn_writeROV IN FRAME actuate_edit /* Create new .ROV file */
DO:
  DEFINE VAR l_ok              AS LOGICAL   NO-UNDO.
  DEFINE VAR filename          AS CHARACTER NO-UNDO.
  DEFINE VAR Filter_NameString AS CHARACTER NO-UNDO.
  DEFINE VAR Filter_FileSpec   AS CHARACTER NO-UNDO.

  /* Initialize the file filter */
  ASSIGN Filter_NameString = "Report parameter values (*.rov)"
         Filter_FileSpec   = "*.rov":U. 

  /* Ask for a file name. NOTE: File-names to run must exist */                          
  filename = ENTRY(2,pc_filename:SCREEN-VALUE,".") + ".rov":U.
  SYSTEM-DIALOG GET-FILE filename
      TITLE    "Write parameter file"
      FILTERS  Filter_NameString Filter_FileSpec
      UPDATE   l_ok IN WINDOW {&WINDOW-NAME}.  
  IF l_ok THEN
    RUN AcReqWriteFile(INPUT fileNumber, INPUT filename).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME GroupName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL GroupName actuate_edit
ON VALUE-CHANGED OF GroupName IN FRAME actuate_edit /* Group */
DO:
  /* Clear Parameter Name combo-box */
  ASSIGN ParamName:LIST-ITEMS = ?.
  
  /* 
   * Load up Parameter Name combo-box with all of the names
   * for the chosen group
   */
  FOR EACH rptparams WHERE p_group = (IF SELF:SCREEN-VALUE = ? THEN "" ELSE SELF:SCREEN-VALUE):
    ParamName:ADD-LAST(rptparams.p_name).
  END.
  
  /* 
   * If we loaded some values, set current value to the
   * first value and spark a change.
   */
  IF ParamName:LIST-ITEMS NE ? THEN DO:
    ParamName:SCREEN-VALUE = ParamName:ENTRY(1).
    APPLY "VALUE-CHANGED":U TO ParamName.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ParamName
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ParamName actuate_edit
ON VALUE-CHANGED OF ParamName IN FRAME actuate_edit /* Name */
DO:
  ASSIGN GroupName
         ParamName.
         
  FIND rptparams WHERE p_group = GroupName AND 
                       p_name  = ParamName.

  ASSIGN ParamAlias   = p_Alias
         ParamType    = p_ParmType
         tog_Req      = p_Required
         tog_Hidden   = p_Hidden
         tog_HideText = p_HideText
         tog_AdHoc    = p_AdHoc
         CurrentValue = p_Value.
  
  DISPLAY ParamAlias ParamType tog_Req tog_Hidden tog_HideText tog_AdHoc CurrentValue
    WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pc_filename
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pc_filename actuate_edit
ON RETURN OF pc_filename IN FRAME actuate_edit /* Filename */
DO:
  ASSIGN pc_filename.
  
  /* Clear temp-table */
  FOR EACH rptparams: 
    DELETE rptparams. 
  END.
  
  /* Close any open file */
  RUN AcReqCloseFile(INPUT fileNumber).

  /* Load temp-table with report parameters */
  RUN Load_Report_Parameters.

  /* Clear GroupName combobox */
  GroupName:LIST-ITEMS = ?.
  
  /* Load up Group combobox */
  FOR EACH rptparams BREAK BY p_group:
    IF FIRST-OF(p_group) THEN GroupName:ADD-LAST(rptparams.p_group).
  END.
  
  /* If we had some values, fill in the screen */
  IF GroupName:LIST-ITEMS NE ? THEN DO:
    GroupName:SCREEN-VALUE = GroupName:ENTRY(1).
    APPLY "VALUE-CHANGED":U TO GroupName.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK actuate_edit 


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

  /* Initialize Actuate API */
  hSession = InitializeAPI().
  IF hSession = -1 THEN
    RETURN "ERROR":U. /* Could not load API, exit procedure */

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* Clean up. Close the file and uninitialize the Actuate API */
RUN AcReqCloseFile(INPUT fileNumber).
UnInitializeAPI(hSession).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI actuate_edit _DEFAULT-DISABLE
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
  HIDE FRAME actuate_edit.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI actuate_edit _DEFAULT-ENABLE
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
  DISPLAY pc_filename GroupName ParamName ParamAlias ParamType tog_req 
          tog_HideText tog_Hidden tog_AdHoc CurrentValue 
      WITH FRAME actuate_edit.
  ENABLE pc_filename btn_Browse Btn_OK RECT-2 GroupName ParamName RECT-1 
         CurrentValue btn_SaveValue btn_writeROV btn_generate 
      WITH FRAME actuate_edit.
  VIEW FRAME actuate_edit.
  {&OPEN-BROWSERS-IN-QUERY-actuate_edit}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Load_Report_Parameters actuate_edit 
PROCEDURE Load_Report_Parameters :
/*------------------------------------------------------------------------------
  Purpose:     Load_Report_Parameters
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE pcurrentGroup     AS MEMPTR.
    
    /* If a valid file, rox or rov, read each group and parameters */        
    IF INDEX(pc_filename, ".rox":U) <> 0 OR
       INDEX(pc_filename, ".rov":U) <> 0 THEN
    DO:
      /* Open the specified ROX or ROV file */
      RUN AcReqReadFile(INPUT pc_filename, OUTPUT fileNumber).   

      /* Allocate memory for pointers */
      SET-SIZE(pcurrentGroup)     = 128.
        
      /* Get the first group in the file */
      RUN AcReqGetFirstGroup(INPUT fileNumber, OUTPUT pcurrentGroup).
      ASSIGN currentGroup = GET-STRING(pcurrentGroup,1).

      /* Read the parameters for each group */
      DO WHILE currentGroup NE "":
        RUN Process_Parameters(INPUT currentGroup).
        RUN AcReqGetNextGroup(INPUT fileNumber, OUTPUT pcurrentGroup).
        ASSIGN currentGroup = GET-STRING(pcurrentGroup,1).   
      END.

      /* Find parameters which do not belong to a group */
      ASSIGN currentGroup = "". /* No group */
      RUN Process_Parameters(INPUT currentGroup).
            
      /* Free memory */
      SET-SIZE(pcurrentGroup) = 0.
    END.
    ELSE
      MESSAGE "The file is not an Actuate ROX or ROV file" VIEW-AS ALERT-BOX ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Process_Parameters actuate_edit 
PROCEDURE Process_Parameters :
/*------------------------------------------------------------------------------
  Purpose:     Reads parameters and loads them into the temp-table
  Parameters:  currentGroup (char) - the name parameters are grouped under
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER currentGroup AS CHARACTER NO-UNDO.

  DEFINE VARIABLE pcurrentParameter AS MEMPTR  NO-UNDO.  
  DEFINE VARIABLE pAlias            AS MEMPTR  NO-UNDO.
  DEFINE VARIABLE pParmType         AS MEMPTR  NO-UNDO.
  DEFINE VARIABLE pCharValue        AS MEMPTR  NO-UNDO.
  DEFINE VARIABLE DblValue          AS DECIMAL NO-UNDO.
  DEFINE VARIABLE IntValue          AS INTEGER NO-UNDO.
  
  /* Allocate memory */
  SET-SIZE(pcurrentParameter) = 128.
  SET-SIZE(pAlias)            = 128.
  SET-SIZE(pParmType)         = 128.
  SET-SIZE(pCharValue)        = 128.

  RUN AcReqGetFirstParameter(INPUT fileNumber, INPUT currentGroup, OUTPUT pcurrentParameter).
  currentParameter = GET-STRING(pcurrentParameter,1).
  DO WHILE currentParameter NE "":
    FIND rptparams WHERE p_name = currentParameter NO-ERROR.
    IF NOT AVAILABLE rptparams THEN DO:
      CREATE rptparams.
      ASSIGN rptparams.p_group = currentGroup
             rptparams.p_name  = currentParameter.
      RUN AcReqGetAlias        (INPUT fileNumber, INPUT currentParameter, OUTPUT pAlias).
      rptparams.p_alias    = GET-STRING(pAlias,1).
      RUN AcReqGetParmType     (INPUT fileNumber, INPUT currentParameter, OUTPUT pParmType).
      rptparams.p_ParmType = GET-STRING(pParmType,1).
      RUN AcReqGetRequired     (INPUT fileNumber, INPUT currentParameter, OUTPUT IntValue).
      rptparams.p_Required = (IF IntValue = 0 THEN NO ELSE YES).
      RUN AcReqGetHidden       (INPUT fileNumber, INPUT currentParameter, OUTPUT IntValue).
      rptparams.p_Hidden   = (IF IntValue = 0 THEN NO ELSE YES).
      RUN AcReqGetHideText     (INPUT fileNumber, INPUT currentParameter, OUTPUT IntValue).
      rptparams.p_HideText = (IF IntValue = 0 THEN NO ELSE YES).
      RUN AcReqGetAdHoc        (INPUT fileNumber, INPUT currentParameter, OUTPUT IntValue).
      rptparams.p_AdHoc    = (IF IntValue = 0 THEN NO ELSE YES).
    
      CASE rptparams.p_ParmType:
        WHEN "String":U   THEN DO:
          RUN AcReqGetValueString(INPUT fileNumber, INPUT currentParameter, OUTPUT pCharValue).
          rptparams.p_Value = GET-STRING(pCharValue,1).
        END.
        WHEN "Double":U THEN DO:
          RUN AcReqGetValueDouble(INPUT fileNumber, INPUT currentParameter, OUTPUT DblValue).
          rptparams.p_Value = STRING(DblValue).
        END.
        WHEN "Integer":U THEN DO:
          RUN AcReqGetValueInteger(INPUT fileNumber, INPUT currentParameter, OUTPUT IntValue).
          rptparams.p_Value = STRING(IntValue).          
        END.
        WHEN "Date":U     OR
        WHEN "Currency":U THEN DO:
          RUN AcReqGetValueStr(INPUT fileNumber, INPUT currentParameter, OUTPUT pCharValue).
          rptparams.p_Value = GET-STRING(pCharValue,1).
        END.
      END CASE.
    END.
    RUN AcReqGetNextParameter(INPUT fileNumber, INPUT currentGroup,     OUTPUT pcurrentParameter).
    currentParameter = GET-STRING(pcurrentParameter,1). 
  END.
  
  /* Free memory */
  SET-SIZE(pcurrentParameter) = 0.
  SET-SIZE(pAlias)            = 0.
  SET-SIZE(pParmType)         = 0.
  SET-SIZE(pCharValue)        = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


