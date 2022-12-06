/**************************************************************************
    Procedure: acgenrpt.p

    Purpose  : Calls the Actuate API to generate a report (.ROI)
               from a Actuate parameter file (.ROV) or Executable (.ROX)

    Notes    :
    Author   : Gerry Seidl
    Date     : 6/30/97
    
    Modified on .
    
**************************************************************************/
/* INCLUDE FILES */
{samples/actuate/acapi.i} /* Actuate API definitions */

/* INPUT PARAMETERS */
DEFINE INPUT PARAMETER pc_filename AS CHARACTER NO-UNDO. /* ROX or ROV file */
DEFINE INPUT PARAMETER pc_EUDTPath AS CHARACTER NO-UNDO. /* End User Desktop Path */
DEFINE INPUT PARAMETER pi_Options  AS INTEGER   NO-UNDO. /* Options */

/* OUTPUT PARAMETERS */
DEFINE OUTPUT PARAMETER pi_status  AS INTEGER   NO-UNDO. /* return code */

/* LOCAL VARIABLES */
DEFINE VARIABLE filenumber AS INTEGER   NO-UNDO.
DEFINE VARIABLE errmsg     AS CHARACTER NO-UNDO.

/* Initialize Actuate API */
hSession = InitializeAPI().
IF hSession = -1 THEN DO:
  pi_Status = -1.
  RETURN "ERROR":U. /* Could not load API, exit procedure */
END.

/* If specified, Set End User Desktop path. */
IF pc_EUDTPath NE "" AND pc_EUDTPath NE ? THEN
  RUN AcReqSetEUDTPath(pc_EUDTPath).
  
/* 
 * If the file specified was an "ROX" file, then open it 
 * and use the file number in the call to AcReqGenerateReport
 */
IF INDEX(pc_filename, ".rox":U) <> 0 THEN DO:
  RUN AcReqReadFile(INPUT pc_filename, OUTPUT filenumber).
  RUN CheckForAPIError(INPUT filenumber, OUTPUT pi_Status, OUTPUT errmsg).
  IF pi_Status > 0 THEN DO:
    /* If error, display error message and bail out */
    MESSAGE errmsg VIEW-AS ALERT-BOX ERROR TITLE "Actuate API Error".
    UnInitializeAPI(hSession).
    RETURN "ERROR":U.
  END.
END.
ELSE filenumber = 0.
  
/* 
 * Generate the report. Note that the one of the imbedded parameters 
 * is the name of the ROI to generate. 
 */
RUN AcReqGenerateReport(pc_fileName, pi_Options, filenumber, OUTPUT pi_Status).

IF filenumber <> 0 THEN RUN AcReqCloseFile(INPUT filenumber).

/* Shutdown the Actuate API */            
UnInitializeAPI(hSession).

RETURN.


