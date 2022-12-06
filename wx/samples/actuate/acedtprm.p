/**************************************************************************
    Procedure: acedtprm.p

    Purpose  : Modifies the parameters stored in a ROV or ROX file and
               saves them to a new ROV file.

    Notes    : Param/Value pairs are sent as a CHR(10) delimited list
               (e.g. "Param1='Value1'" + CHR(10) + "Param2=Value2" )
    Author   : Gerry Seidl
    Date     : 6/30/97
    
    Modified on .
    
**************************************************************************/
/* INCLUDE FILES */
{samples/actuate/acapi.i} /* Actuate API definitions */

/* INPUT PARAMETERS */
DEFINE INPUT PARAMETER pc_filename   AS CHARACTER NO-UNDO. /* ROX or ROV file */
DEFINE INPUT PARAMETER pc_valuepairs AS CHARACTER NO-UNDO. /* Param/Value pairs */
DEFINE INPUT PARAMETER pc_rovfile    AS CHARACTER NO-UNDO. /* ROV file to save to */

/* LOCAL VARIABLES */
DEFINE VARIABLE rc               AS INTEGER   NO-UNDO.
DEFINE VARIABLE fileNumber       AS INTEGER   NO-UNDO.
DEFINE VARIABLE currentGroup     AS CHARACTER NO-UNDO.
DEFINE VARIABLE currentParameter AS CHARACTER NO-UNDO.  
DEFINE VARIABLE i                AS INTEGER   NO-UNDO.
DEFINE VARIABLE ParamName        AS CHARACTER NO-UNDO.  
DEFINE VARIABLE ParamValue       AS CHARACTER NO-UNDO.  

/* TEMP-TABLE DEFINITIONS */
DEFINE TEMP-TABLE rptparams 
  FIELD p_name     AS CHARACTER FORMAT "X(128)" CASE-SENSITIVE
  FIELD p_alias    AS CHARACTER FORMAT "X(128)"
  FIELD p_required AS LOGICAL
  FIELD p_hidden   AS LOGICAL
  FIELD p_HideText AS LOGICAL
  FIELD p_AdHoc    AS LOGICAL
  FIELD p_ParmType AS CHARACTER FORMAT "X(15)"
  FIELD p_value    AS CHARACTER
  INDEX name IS PRIMARY p_name.

/* Initialize Actuate API */
hSession = InitializeAPI().
IF hSession = -1 THEN 
  RETURN "ERROR":U. /* Could not load API, exit procedure */

/* 
 * Read all parameters from specified file and 
 * load them into the temp-table.
 */
RUN Load_Report_Parameters.

/* 
 * If there was an error loading the parameters then
 * shut down the API and return an error status 
 */  
IF RETURN-VALUE = "ERROR":U THEN DO:
  UnInitializeAPI(hSession).
  RETURN "ERROR":U.
END.

/* 
 * Iterate through the param/value pairs (e.g. Param1=value )
 * and set the new parameter values.
 */
DO i = 1 TO NUM-ENTRIES(pc_valuepairs,CHR(10)):
  ASSIGN ParamName  = ENTRY(1,ENTRY(i,pc_valuepairs,CHR(10)),"=":U)
         ParamValue = ENTRY(2,ENTRY(i,pc_valuepairs,CHR(10)),"=":U).
  FIND rptparams WHERE p_name = ParamName NO-ERROR.
  IF AVAILABLE rptparams THEN DO:
    /* Set the parameter to the new value */
    CASE rptparams.p_ParmType:
      WHEN "String":U   OR
      WHEN "Currency":U OR
      WHEN "Date":U     THEN RUN AcReqSetValueString  (INPUT fileNumber, INPUT ParamName, INPUT ParamValue).
      WHEN "Integer":U  THEN RUN AcReqSetValueInteger (INPUT fileNumber, INPUT ParamName, INPUT INTEGER(ParamValue)).
      WHEN "Double":U   THEN RUN AcReqSetValueDouble  (INPUT fileNumber, INPUT ParamName, INPUT DECIMAL(ParamValue)).
    END CASE.
  END.
  ELSE DO:
    MESSAGE "The parameter: " + ParamName + " was not found in file: " + pc_filename 
      VIEW-AS ALERT-BOX ERROR TITLE "Parameter error from acedtprm.p".
    rc = 1. /* set flag for error status */
  END.
END.
    
/* Save parameters to ROV file */
RUN AcReqWriteFile(INPUT fileNumber, INPUT pc_rovfile).

RUN AcReqCloseFile(INPUT fileNumber).

UnInitializeAPI(hSession).

/* Return "ERROR" if there was an error above */
IF rc <> 0 THEN 
  RETURN "ERROR":U.
ELSE
  RETURN.


PROCEDURE Load_Report_Parameters :
/*------------------------------------------------------------------------------
  Purpose:     Load_Report_Parameters
  Parameters:  <none>
  Notes:       Loads report parameters into the temp-table.
------------------------------------------------------------------------------*/
    DEFINE VARIABLE pcurrentParameter AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE pAlias            AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE pParmType         AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE pCharValue        AS MEMPTR    NO-UNDO.
    DEFINE VARIABLE DblValue          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE i                 AS INTEGER   NO-UNDO.
    DEFINE VARIABLE errmsg            AS CHARACTER NO-UNDO.
              
    /* Read parameters only from ROX or ROV files */
    IF INDEX(pc_filename, ".rox":U) <> 0 OR
       INDEX(pc_filename, ".rov":U) <> 0 THEN
    DO:
      RUN AcReqReadFile(INPUT pc_filename, OUTPUT fileNumber). /* open the file */ 
      RUN CheckForAPIError(INPUT fileNumber, OUTPUT rc, OUTPUT errmsg).
      IF rc NE 0 THEN DO:
        /* If error, display error message and bail out */
        MESSAGE errmsg VIEW-AS ALERT-BOX ERROR TITLE "Actuate API Error".
        RETURN "ERROR":U.
      END.
      
      /* Allocate memory for pointers */
      SET-SIZE(pcurrentParameter) = 128.
      SET-SIZE(pAlias)            = 128.
      SET-SIZE(pParmType)         = 128.
      SET-SIZE(pCharValue)        = 128.
      
      /* Read all parameters */
      ASSIGN currentGroup = "".
      RUN AcReqGetFirstParameter(INPUT fileNumber, INPUT currentGroup, OUTPUT pcurrentParameter).
      currentParameter = GET-STRING(pcurrentParameter,1).
      DO WHILE currentParameter NE "":U:
          CREATE rptparams.
          ASSIGN rptparams.p_name  = currentParameter.
          RUN AcReqGetAlias        (INPUT fileNumber, INPUT currentParameter, OUTPUT pAlias).
          rptparams.p_alias    = GET-STRING(pAlias,1).
          RUN AcReqGetParmType     (INPUT fileNumber, INPUT currentParameter, OUTPUT pParmType).
          rptparams.p_ParmType = GET-STRING(pParmType,1).
          RUN AcReqGetRequired     (INPUT fileNumber, INPUT currentParameter, OUTPUT i).
          rptparams.p_Required = (IF i = 0 THEN NO ELSE YES).
          RUN AcReqGetHidden       (INPUT fileNumber, INPUT currentParameter, OUTPUT i).
          rptparams.p_Hidden   = (IF i = 0 THEN NO ELSE YES).
          RUN AcReqGetHideText     (INPUT fileNumber, INPUT currentParameter, OUTPUT i).
          rptparams.p_HideText = (IF i = 0 THEN NO ELSE YES).
          RUN AcReqGetAdHoc        (INPUT fileNumber, INPUT currentParameter, OUTPUT i).
          rptparams.p_AdHoc    = (IF i = 0 THEN NO ELSE YES).

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
              RUN AcReqGetValueInteger(INPUT fileNumber, INPUT currentParameter, OUTPUT i).
              rptparams.p_Value = STRING(i).          
            END.
            WHEN "Date":U     OR
            WHEN "Currency":U THEN DO:
              RUN AcReqGetValueStr(INPUT fileNumber, INPUT currentParameter, OUTPUT pCharValue).
              rptparams.p_Value = GET-STRING(pCharValue,1).
            END.
          END CASE.
          RUN AcReqGetNextParameter(INPUT fileNumber, INPUT currentGroup,  OUTPUT pcurrentParameter).
          currentParameter = GET-STRING(pcurrentParameter,1). 
      END.
      
      /* Free memory */
      SET-SIZE(pcurrentParameter) = 0.
      SET-SIZE(pAlias)            = 0.
      SET-SIZE(pParmType)         = 0.
      SET-SIZE(pCharValue)        = 0.
    END.
    ELSE DO:
      MESSAGE "The file is not an Actuate ROX or ROV file" VIEW-AS ALERT-BOX ERROR.
      RETURN "ERROR":U.
    END.
END PROCEDURE.

